/* Expressions, statements, alternatives and right-hand sides.
 *
 *   exp      ::= infixexp :: type | infixexp
 *   infixexp ::= [-] lexp (qop [-] lexp)*       -- operators in source order
 *   lexp     ::= \ apat+ -> exp | \case { alts } | let { decls } in exp
 *              | if exp [;] then exp [;] else exp | case exp of { alts }
 *              | do { stmts } | fexp
 *   fexp     ::= aexp (aexp | @ atype)*
 *   aexp     ::= qvar | gcon | literal | _ | ( ... ) | [ ... ]
 *              | qcon { fbinds } | aexp { fbinds }
 *
 * Operator chains stay flat and in source order. Fixity resolution is a
 * later pass, after imports are known. The printer reproduces the chain
 * as written, so any parser resolves the printed source the same way as
 * the original.
 */

#include "parse.h"

static Expr *lexp(Parser *p);

/* The chain of first and steps (InfixStep *), or first alone. */
static Expr *build_chain(Expr *first, Vec *steps)
{
    Expr *e;
    if (steps->len == 0) {
        return first;
    }
    e = expr_new(EXPR_INFIX);
    e->a = first;
    e->steps = *steps;
    return e;
}

static InfixStep *step_new(QName *op, Expr *operand)
{
    InfixStep *s = (InfixStep *)xcalloc(1, sizeof(InfixStep));
    s->op = *op;
    s->operand = operand;
    return s;
}

/* One operand of an operator chain: a lexp, with optional prefix
 * negation. */
static Expr *operand(Parser *p)
{
    if (p_at_varsym(p, "-")) {
        Expr *e = expr_new(EXPR_NEG);
        Pos start = p_pos(p);
        p_bump(p);
        e->a = lexp(p);
        e->span = p_span_from(p, start);
        return e->a ? e : NULL;
    }
    return lexp(p);
}

/* An operator chain. With allow_section, the chain stops before an
 * operator that is followed by `)`, and sets *section to that operator,
 * so the caller can build a left section. */
static Expr *infixexp_or_section(Parser *p, int allow_section, QName **section)
{
    Expr *first = operand(p);
    Vec rest;
    QName *op;
    size_t len;
    *section = NULL;
    if (first == NULL) {
        return NULL;
    }
    vec_init(&rest);
    while ((op = p_operator_ahead(p, &len)) != NULL) {
        Expr *e;
        if (allow_section && tok_is_special(p_tok_at(p, p->idx + len), ')')) {
            p->idx += len;
            *section = op;
            return build_chain(first, &rest);
        }
        p->idx += len;
        e = operand(p);
        if (e == NULL) {
            return NULL;
        }
        vec_push(&rest, step_new(op, e));
    }
    return build_chain(first, &rest);
}

static Expr *infixexp(Parser *p)
{
    QName *section;
    return infixexp_or_section(p, 0, &section);
}

/* A full expression, with an optional type signature. */
Expr *p_exp(Parser *p)
{
    Expr *e = infixexp(p);
    if (e == NULL) {
        return NULL;
    }
    if (p_eat_op(p, OP_DOUBLECOLON)) {
        Expr *sig = expr_new(EXPR_SIG);
        sig->a = e;
        sig->ty = p_ty(p);
        return sig->ty ? sig : NULL;
    }
    return e;
}

/* An operator at the current position, not consumed, with the number of
 * tokens it takes: `+`, `M.+`, `:`, `:|`, `f` or `M.f` in backquotes. */
QName *p_operator_ahead(const Parser *p, size_t *len)
{
    Span span = p_token_span(p);
    const Token *t = p_peek(p);
    if (t->kind == TOK_VARSYM || t->kind == TOK_CONSYM) {
        *len = 1;
        return qname_new(t->qual, t->text, span);
    }
    if (t->kind == TOK_RESERVEDOP && t->ch == OP_COLON) {
        *len = 1;
        return qname_new("", ":", span);
    }
    if (tok_is_special(t, '`')) {
        const Token *inner = p_tok_at(p, p->idx + 1);
        Span s;
        if (inner == NULL || (inner->kind != TOK_VARID && inner->kind != TOK_CONID)) {
            return NULL;
        }
        if (!tok_is_special(p_tok_at(p, p->idx + 2), '`')) {
            return NULL;
        }
        s.start = inner->pos;
        s.end = inner->end;
        *len = 3;
        return qname_new(inner->qual, inner->text, s);
    }
    return NULL;
}

/* DoAndIfThenElse: a `;` may stand before `then` and `else`. */
static void semis_before(Parser *p, Keyword k)
{
    const Token *next;
    if (!(p_at_special(p, ';') || p_kind(p) == TOK_VSEMI)) {
        return;
    }
    next = p_tok_at(p, p->idx + 1);
    if (next != NULL && next->kind == TOK_KEYWORD && next->ch == (unsigned long)k) {
        p_bump(p);
    }
}

static void *decl_item(Parser *p, void *env)
{
    (void)env;
    return p_decl(p, CTX_LOCAL);
}

/* The declarations of a `let` or `where`, in a block. */
int p_local_decls(Parser *p, Vec *decls)
{
    return p_block(p, decl_item, NULL, decls);
}

static void *alt_item(Parser *p, void *env)
{
    Alt *a = (Alt *)xcalloc(1, sizeof(Alt));
    (void)env;
    a->pos = p_pos(p);
    a->pat = p_pat(p);
    if (a->pat == NULL) {
        return NULL;
    }
    a->rhs = p_rhs(p, OP_RIGHTARROW);
    return a->rhs ? a : NULL;
}

static int alts(Parser *p, Vec *out)
{
    return p_block(p, alt_item, NULL, out);
}

static void *stmt_item(Parser *p, void *env)
{
    (void)env;
    return p_stmt(p);
}

/* The statements of a `do` block. */
static int stmts(Parser *p, Vec *out)
{
    return p_block(p, stmt_item, NULL, out);
}

/* An expression with a keyword at the front, or an application. */
static Expr *fexp(Parser *p);

/* One alternative of a multi-way if: `| g1, g2 -> e`. */
static void *multi_if_alt(Parser *p, void *env)
{
    GuardedRhs *g = (GuardedRhs *)xcalloc(1, sizeof(GuardedRhs));
    (void)env;
    if (p_expect_op(p, OP_BAR, "`|`") != 0) {
        return NULL;
    }
    for (;;) {
        Stmt *s = p_stmt(p);
        if (s == NULL) {
            return NULL;
        }
        vec_push(&g->guards, s);
        if (!p_eat_special(p, ',')) {
            break;
        }
    }
    if (p_expect_op(p, OP_RIGHTARROW, "`->`") != 0) {
        return NULL;
    }
    g->body = p_exp(p);
    return g->body ? g : NULL;
}

static Expr *lexp(Parser *p)
{
    Expr *e;
    if (p_at_op(p, OP_BACKSLASH)) {
        p_bump(p);
        if (p_at_keyword(p, KW_CASE)) {
            p_bump(p);
            e = expr_new(EXPR_LAMBDA_CASE);
            return alts(p, &e->alts) == 0 ? e : NULL;
        }
        e = expr_new(EXPR_LAMBDA);
        while (!p_at_op(p, OP_RIGHTARROW)) {
            Pat *pat = p_apat(p);
            if (pat == NULL) {
                return NULL;
            }
            vec_push(&e->pats, pat);
        }
        p_bump(p);
        e->a = p_exp(p);
        return e->a ? e : NULL;
    }
    if (p_at_keyword(p, KW_LET)) {
        p_bump(p);
        e = expr_new(EXPR_LET);
        if (p_local_decls(p, &e->decls) != 0 || p_expect_keyword(p, KW_IN) != 0) {
            return NULL;
        }
        e->a = p_exp(p);
        return e->a ? e : NULL;
    }
    if (p_at_keyword(p, KW_IF)) {
        const Token *next;
        p_bump(p);
        next = p_tok_at(p, p->idx + 1);
        if ((p_kind(p) == TOK_VOPEN || p_at_special(p, '{')) && next != NULL
            && next->kind == TOK_RESERVEDOP && next->ch == OP_BAR) {
            e = expr_new(EXPR_MULTI_IF);
            return p_guard_block(p, multi_if_alt, NULL, &e->guards) == 0 ? e : NULL;
        }
        e = expr_new(EXPR_IF);
        e->a = p_exp(p);
        if (e->a == NULL) {
            return NULL;
        }
        semis_before(p, KW_THEN);
        if (p_expect_keyword(p, KW_THEN) != 0) {
            return NULL;
        }
        e->b = p_exp(p);
        if (e->b == NULL) {
            return NULL;
        }
        semis_before(p, KW_ELSE);
        if (p_expect_keyword(p, KW_ELSE) != 0) {
            return NULL;
        }
        e->c = p_exp(p);
        return e->c ? e : NULL;
    }
    if (p_at_keyword(p, KW_CASE)) {
        p_bump(p);
        e = expr_new(EXPR_CASE);
        e->a = p_exp(p);
        if (e->a == NULL || p_expect_keyword(p, KW_OF) != 0) {
            return NULL;
        }
        return alts(p, &e->alts) == 0 ? e : NULL;
    }
    if (p_at_keyword(p, KW_DO)) {
        p_bump(p);
        e = expr_new(EXPR_DO);
        return stmts(p, &e->stmts) == 0 ? e : NULL;
    }
    return fexp(p);
}

static int at_aexp_start(const Parser *p)
{
    const Token *t = p_peek(p);
    switch (t->kind) {
    case TOK_VARID:
    case TOK_CONID:
    case TOK_INTEGER:
    case TOK_FLOAT:
    case TOK_CHAR:
    case TOK_STRING:
        return 1;
    case TOK_SPECIAL:
        return t->ch == '(' || t->ch == '[';
    case TOK_KEYWORD:
        return t->ch == KW_UNDERSCORE;
    default:
        return 0;
    }
}

static Expr *aexp(Parser *p);

/* A function application. */
static Expr *fexp(Parser *p)
{
    Expr *e = aexp(p);
    if (e == NULL) {
        return NULL;
    }
    for (;;) {
        Expr *app;
        if (p_at_op(p, OP_AT)) {
            p_bump(p);
            app = expr_new(EXPR_TYPE_APP);
            app->a = e;
            app->ty = p_atype(p);
            if (app->ty == NULL) {
                return NULL;
            }
            e = app;
        } else if (at_aexp_start(p)) {
            app = expr_new(EXPR_APP);
            app->a = e;
            app->b = aexp(p);
            if (app->b == NULL) {
                return NULL;
            }
            e = app;
        } else {
            return e;
        }
    }
}

/* `{ f = e, g, .. }` */
static int field_updates(Parser *p, Vec *fields, int *wildcard)
{
    vec_init(fields);
    *wildcard = 0;
    if (p_expect_special(p, '{') != 0) {
        return -1;
    }
    if (!p_at_special(p, '}')) {
        for (;;) {
            FieldUpdate *f;
            QName *name;
            if (p_eat_op(p, OP_DOTDOT)) {
                *wildcard = 1;
                break;
            }
            name = p_qname(p);
            if (name == NULL) {
                return -1;
            }
            f = (FieldUpdate *)xcalloc(1, sizeof(FieldUpdate));
            f->name = *name;
            if (p_eat_op(p, OP_EQUALS)) {
                f->value = p_exp(p);
                if (f->value == NULL) {
                    return -1;
                }
            }
            vec_push(fields, f);
            if (!p_eat_special(p, ',')) {
                break;
            }
        }
    }
    return p_expect_special(p, '}');
}

static Expr *paren_exp(Parser *p);
static Expr *bracket_exp(Parser *p);

static Expr *name_expr(ExprKind kind, QName *name)
{
    Expr *e = expr_new(kind);
    e->name = *name;
    return e;
}

static Expr *aexp1(Parser *p)
{
    const Token *t = p_peek(p);
    QName *name;
    Expr *e;
    switch (t->kind) {
    case TOK_VARID:
        name = qname_new(t->qual, t->text, p_token_span(p));
        p_bump(p);
        return name_expr(EXPR_VAR, name);
    case TOK_KEYWORD:
        if (t->ch != KW_UNDERSCORE) {
            break;
        }
        p_bump(p);
        return expr_new(EXPR_HOLE);
    case TOK_INTEGER:
    case TOK_FLOAT:
    case TOK_CHAR:
    case TOK_STRING:
        e = expr_new(EXPR_LIT);
        e->span = p_token_span(p);
        return p_literal(p, &e->lit) == 0 ? e : NULL;
    case TOK_CONID:
        return name_expr(EXPR_CON, p_gcon(p));
    case TOK_SPECIAL:
        if (t->ch == '(') {
            name = p_gcon(p);
            if (name != NULL) {
                return name_expr(EXPR_CON, name);
            }
            name = p_parenthesized_name(p);
            if (name != NULL) {
                return name_expr(EXPR_VAR, name);
            }
            return paren_exp(p);
        }
        if (t->ch == '[') {
            name = p_gcon(p);
            if (name != NULL) {
                return name_expr(EXPR_CON, name);
            }
            return bracket_exp(p);
        }
        break;
    default:
        break;
    }
    p_unexpected(p, "an expression");
    return NULL;
}

/* An atomic expression, with any record braces that follow it. */
static Expr *aexp(Parser *p)
{
    Expr *e = aexp1(p);
    if (e == NULL) {
        return NULL;
    }
    while (p_at_special(p, '{')) {
        Vec fields;
        int wildcard;
        if (field_updates(p, &fields, &wildcard) != 0) {
            return NULL;
        }
        if (e->kind == EXPR_CON) {
            e->kind = EXPR_RECORD_CON;
            e->fields = fields;
            e->wildcard = wildcard;
        } else {
            Expr *u;
            if (wildcard) {
                p_error(p, xstrdup("`..` in a record update"));
                return NULL;
            }
            u = expr_new(EXPR_RECORD_UPDATE);
            u->a = e;
            u->fields = fields;
            e = u;
        }
    }
    return e;
}

/* After `(`: a parenthesized expression, a tuple, a tuple section, or an
 * operator section. */
static Expr *paren_exp(Parser *p)
{
    QName *op;
    size_t len, i;
    Vec items;
    int all_some = 1;
    Expr *r;
    Pos start = p_pos(p);
    if (p_expect_special(p, '(') != 0) {
        return NULL;
    }
    /* A right section: `(op e)`. `(- e)` is negation, not a section. */
    op = p_operator_ahead(p, &len);
    if (op != NULL && !(op->qual[0] == '\0' && str_eq(op->name, "-"))) {
        p->idx += len;
        r = expr_new(EXPR_RIGHT_SECTION);
        r->name = *op;
        r->a = infixexp(p);
        if (r->a == NULL || p_expect_special(p, ')') != 0) {
            return NULL;
        }
        return r;
    }
    vec_init(&items);
    for (;;) {
        if (p_at_special(p, ',') || p_at_special(p, ')')) {
            vec_push(&items, NULL);
        } else {
            QName *section;
            Expr *e = infixexp_or_section(p, 1, &section);
            if (e == NULL) {
                return NULL;
            }
            if (section != NULL) {
                if (p_expect_special(p, ')') != 0) {
                    return NULL;
                }
                r = expr_new(EXPR_LEFT_SECTION);
                r->a = e;
                r->name = *section;
                return r;
            }
            if (p_eat_op(p, OP_DOUBLECOLON)) {
                Expr *sig = expr_new(EXPR_SIG);
                sig->a = e;
                sig->ty = p_ty(p);
                if (sig->ty == NULL) {
                    return NULL;
                }
                e = sig;
            }
            vec_push(&items, e);
        }
        if (!p_eat_special(p, ',')) {
            if (p_expect_special(p, ')') != 0) {
                return NULL;
            }
            break;
        }
    }
    if (items.len == 1) {
        if (items.items[0] == NULL) {
            p_error(p, xstrdup("empty parentheses"));
            return NULL;
        }
        r = expr_new(EXPR_PAREN);
        r->a = (Expr *)items.items[0];
        if (r->a->kind == EXPR_NEG) {
            /* As in aihc-parser, `(- e)` has the span of the
             * parentheses. */
            r->a->span = p_span_from(p, start);
        }
        return r;
    }
    for (i = 0; i < items.len; i++) {
        if (items.items[i] == NULL) {
            all_some = 0;
        }
    }
    r = expr_new(all_some ? EXPR_TUPLE : EXPR_TUPLE_SECTION);
    r->items = items;
    r->span = p_span_from(p, start);
    return r;
}

/* The end of an arithmetic sequence after `..`: an optional bound, then
 * `]`. */
static int seq_end(Parser *p, Expr **to)
{
    *to = NULL;
    if (!p_at_special(p, ']')) {
        *to = p_exp(p);
        if (*to == NULL) {
            return -1;
        }
    }
    return p_expect_special(p, ']');
}

/* After `[`: a list, an arithmetic sequence or a list comprehension. */
static Expr *bracket_exp(Parser *p)
{
    Expr *first, *r;
    Pos start = p_pos(p);
    if (p_expect_special(p, '[') != 0) {
        return NULL;
    }
    first = p_exp(p);
    if (first == NULL) {
        return NULL;
    }
    if (p_eat_op(p, OP_DOTDOT)) {
        r = expr_new(EXPR_ARITH_SEQ);
        r->a = first;
        return seq_end(p, &r->c) == 0 ? r : NULL;
    }
    if (p_eat_op(p, OP_BAR)) {
        r = expr_new(EXPR_LIST_COMP);
        r->a = first;
        for (;;) {
            Stmt *s = p_stmt(p);
            if (s == NULL) {
                return NULL;
            }
            vec_push(&r->stmts, s);
            if (!p_eat_special(p, ',')) {
                break;
            }
        }
        return p_expect_special(p, ']') == 0 ? r : NULL;
    }
    r = expr_new(EXPR_LIST);
    vec_push(&r->items, first);
    if (p_eat_special(p, ',')) {
        Expr *second = p_exp(p);
        if (second == NULL) {
            return NULL;
        }
        if (p_eat_op(p, OP_DOTDOT)) {
            r = expr_new(EXPR_ARITH_SEQ);
            r->a = first;
            r->b = second;
            return seq_end(p, &r->c) == 0 ? r : NULL;
        }
        vec_push(&r->items, second);
        while (p_eat_special(p, ',')) {
            Expr *e = p_exp(p);
            if (e == NULL) {
                return NULL;
            }
            vec_push(&r->items, e);
        }
    }
    if (p_expect_special(p, ']') != 0) {
        return NULL;
    }
    r->span = p_span_from(p, start);
    return r;
}

/* --- Statements -------------------------------------------------------- */

/* Whether a `<-` follows at bracket depth zero before the end of this
 * statement. */
static int bind_arrow_ahead(const Parser *p)
{
    size_t i = p->idx;
    unsigned long depth = 0;
    const Token *t;
    while ((t = p_tok_at(p, i)) != NULL) {
        int is_op = t->kind == TOK_RESERVEDOP;
        if (tok_is_special(t, '(') || tok_is_special(t, '[') || tok_is_special(t, '{') ||
            t->kind == TOK_VOPEN) {
            depth++;
        } else if (tok_is_special(t, ')') || tok_is_special(t, ']') ||
                   tok_is_special(t, '}') || t->kind == TOK_VCLOSE) {
            if (depth == 0) {
                return 0;
            }
            depth--;
        } else if (is_op && t->ch == OP_LEFTARROW && depth == 0) {
            return 1;
        } else if ((tok_is_special(t, ';') || tok_is_special(t, ',') || t->kind == TOK_VSEMI ||
                    t->kind == TOK_EOF) &&
                   depth == 0) {
            return 0;
        } else if (is_op && (t->ch == OP_EQUALS || t->ch == OP_RIGHTARROW || t->ch == OP_BAR) &&
                   depth == 0) {
            return 0;
        }
        i++;
    }
    return 0;
}

/* One statement of a `do` block, a comprehension or a guard. */
static Stmt *p_stmt1(Parser *p)
{
    Stmt *s = (Stmt *)xcalloc(1, sizeof(Stmt));
    if (p_at_keyword(p, KW_LET)) {
        p_bump(p);
        if (p_local_decls(p, &s->decls) != 0) {
            return NULL;
        }
        if (p_eat_keyword(p, KW_IN)) {
            /* `let ... in e` as an expression statement, possibly the
             * first operand of an operator chain. */
            Expr *first = expr_new(EXPR_LET);
            Vec rest;
            QName *op;
            size_t len;
            first->decls = s->decls;
            first->a = p_exp(p);
            if (first->a == NULL) {
                return NULL;
            }
            vec_init(&rest);
            while ((op = p_operator_ahead(p, &len)) != NULL) {
                Expr *e;
                p->idx += len;
                e = operand(p);
                if (e == NULL) {
                    return NULL;
                }
                vec_push(&rest, step_new(op, e));
            }
            vec_init(&s->decls);
            s->kind = STMT_EXPR;
            s->expr = build_chain(first, &rest);
            return s;
        }
        s->kind = STMT_LET;
        return s;
    }
    if (bind_arrow_ahead(p)) {
        s->kind = STMT_BIND;
        s->pat = p_pat(p);
        if (s->pat == NULL || p_expect_op(p, OP_LEFTARROW, "`<-`") != 0) {
            return NULL;
        }
        s->expr = p_exp(p);
        return s->expr ? s : NULL;
    }
    s->kind = STMT_EXPR;
    s->expr = p_exp(p);
    return s->expr ? s : NULL;
}

Stmt *p_stmt(Parser *p)
{
    Pos start = p_pos(p);
    Stmt *s = p_stmt1(p);
    if (s != NULL) {
        s->span = p_span_from(p, start);
    }
    return s;
}

/* --- Right-hand sides -------------------------------------------------- */

/* `= e` or `| g = e ...`, then an optional `where`. sep is `=` in a
 * binding and `->` in an alternative. */
Rhs *p_rhs(Parser *p, ReservedOp sep)
{
    Rhs *r = (Rhs *)xcalloc(1, sizeof(Rhs));
    char *what = str_printf("`%s`", reservedop_str((int)sep));
    if (p_at_op(p, OP_BAR)) {
        r->guarded = 1;
        while (p_eat_op(p, OP_BAR)) {
            GuardedRhs *g = (GuardedRhs *)xcalloc(1, sizeof(GuardedRhs));
            for (;;) {
                Stmt *s = p_stmt(p);
                if (s == NULL) {
                    return NULL;
                }
                vec_push(&g->guards, s);
                if (!p_eat_special(p, ',')) {
                    break;
                }
            }
            if (p_expect_op(p, sep, what) != 0) {
                return NULL;
            }
            g->body = p_exp(p);
            if (g->body == NULL) {
                return NULL;
            }
            vec_push(&r->guards, g);
        }
    } else {
        if (p_expect_op(p, sep, what) != 0) {
            return NULL;
        }
        r->plain = p_exp(p);
        if (r->plain == NULL) {
            return NULL;
        }
    }
    if (p_eat_keyword(p, KW_WHERE)) {
        if (p_local_decls(p, &r->wheres) != 0) {
            return NULL;
        }
    }
    return r;
}
