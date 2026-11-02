/* Declarations: type signatures, fixity, `data`, `newtype`, `type`,
 * `class`, `instance`, bindings and pattern synonyms.
 */

#include "parse.h"

static void *decl_item(Parser *p, void *env)
{
    return p_decl(p, *(DeclContext *)env);
}

/* A `where` block of declarations, which may be empty. The `where`
 * itself is consumed here. */
static int where_decls(Parser *p, DeclContext ctx, Vec *decls)
{
    vec_init(decls);
    if (!p_eat_keyword(p, KW_WHERE)) {
        return 0;
    }
    return p_block(p, decl_item, &ctx, decls);
}

/* A name in a signature: `f` or `(+)`. */
static Ident *sig_name(Parser *p)
{
    QName *name = p_parenthesized_name(p);
    if (name != NULL) {
        return ident_new(name->name, name->span);
    }
    return p_varid(p);
}

/* An unqualified constructor name. */
static Ident *conid(Parser *p)
{
    const Token *t = p_peek(p);
    Ident *name;
    if (t->kind == TOK_CONID && t->qual[0] == '\0') {
        name = ident_new(t->text, p_token_span(p));
        p_bump(p);
        return name;
    }
    p_unexpected(p, "a constructor name");
    return NULL;
}

/* `pattern` starts a pattern synonym when a constructor name, or a
 * variable followed by a constructor operator, comes next. */
static int pattern_decl_ahead(const Parser *p)
{
    const Token *t = p_tok_at(p, p->idx + 1), *u;
    if (t == NULL) {
        return 0;
    }
    if (t->kind == TOK_CONID) {
        return 1;
    }
    if (t->kind == TOK_VARID) {
        u = p_tok_at(p, p->idx + 2);
        return u != NULL && (u->kind == TOK_CONSYM || tok_is_special(u, '`'));
    }
    return 0;
}

static int next_is_varid_or_paren(const Parser *p)
{
    const Token *t = p_tok_at(p, p->idx + 1);
    return t != NULL && (t->kind == TOK_VARID || tok_is_special(t, '('));
}

static int at_lhs_end(const Parser *p)
{
    return p_at_op(p, OP_EQUALS) || p_at_op(p, OP_BAR);
}

/* A variable operator, not consumed, with its token count: `<+>` or `f`
 * in backquotes. Constructor operators belong to patterns. */
static Ident *var_operator_ahead(const Parser *p, size_t *len)
{
    const Token *t = p_peek(p);
    if (t->kind == TOK_VARSYM && t->qual[0] == '\0') {
        *len = 1;
        return ident_new(t->text, p_token_span(p));
    }
    if (tok_is_special(t, '`')) {
        const Token *inner = p_tok_at(p, p->idx + 1);
        Span span;
        if (inner == NULL || inner->kind != TOK_VARID) {
            return NULL;
        }
        if (inner->qual[0] != '\0' || !tok_is_special(p_tok_at(p, p->idx + 2), '`')) {
            return NULL;
        }
        span.start = inner->pos;
        span.end = inner->end;
        *len = 3;
        return ident_new(str_printf("`%s`", inner->text), span);
    }
    return NULL;
}

/* The left-hand side of a binding: `f p1 p2`, `(op) p1`, `p1 op p2` or a
 * pattern. */
static int lhs(Parser *p, Lhs *out)
{
    size_t start = p->idx, len;
    Ident *name = sig_name(p), *op;
    Pat *left;
    vec_init(&out->args);
    if (name != NULL) {
        int failed = 0;
        while (p_at_apat_start(p)) {
            Pat *arg = p_apat(p);
            if (arg == NULL) {
                failed = 1;
                break;
            }
            vec_push(&out->args, arg);
        }
        if (failed) {
            return -1;
        }
        if (at_lhs_end(p)) {
            out->kind = LHS_FUN;
            out->name = *name;
            return 0;
        }
        p->idx = start;
        vec_init(&out->args);
    }
    left = p_pat(p);
    if (left == NULL) {
        return -1;
    }
    if (p_eat_op(p, OP_DOUBLECOLON)) {
        /* A pattern binding with a signature: `p :: t = e`. */
        Pat *sig = pat_new(PAT_SIG);
        sig->a = left;
        sig->ty = p_ty(p);
        if (sig->ty == NULL) {
            return -1;
        }
        left = sig;
    }
    op = var_operator_ahead(p, &len);
    if (op != NULL) {
        p->idx += len;
        out->kind = LHS_INFIX;
        out->left = left;
        out->name = *op;
        out->right = p_pat(p);
        return out->right ? 0 : -1;
    }
    if (at_lhs_end(p)) {
        out->kind = LHS_PAT;
        out->pat = left;
        return 0;
    }
    return p_unexpected(p, "`=` or `|`");
}

/* `f, g :: t`, or a binding. */
static Decl *sig_or_binding(Parser *p)
{
    Pos pos = p_pos(p);
    size_t start = p->idx;
    Decl *d = decl_new(DECL_TYPE_SIG, pos);
    Ident *name;
    while ((name = sig_name(p)) != NULL) {
        vec_push(&d->names, name);
        if (p_eat_special(p, ',')) {
            continue;
        }
        if (p_eat_op(p, OP_DOUBLECOLON)) {
            d->ty = p_ty(p);
            return d->ty ? d : NULL;
        }
        break;
    }
    p->idx = start;
    d = decl_new(DECL_BIND, pos);
    if (lhs(p, &d->lhs) != 0) {
        return NULL;
    }
    d->rhs = p_rhs(p, OP_EQUALS);
    return d->rhs ? d : NULL;
}

/* `pattern P, Q :: t`, or a pattern synonym definition. */
static Decl *pat_syn(Parser *p)
{
    Pos pos = p_pos(p);
    size_t start;
    Ident *name;
    Decl *d;
    p_bump(p);
    start = p->idx;
    d = decl_new(DECL_PAT_SYN_SIG, pos);
    while ((name = conid(p)) != NULL) {
        vec_push(&d->strs, name->name);
        if (!p_eat_special(p, ',')) {
            break;
        }
    }
    if (d->strs.len > 0 && p_eat_op(p, OP_DOUBLECOLON)) {
        d->ty = p_ty(p);
        return d->ty ? d : NULL;
    }
    p->idx = start;
    d = decl_new(DECL_PAT_SYN, pos);
    vec_init(&d->patsyn.args);
    name = conid(p);
    if (name != NULL) {
        d->patsyn.name = name->name;
        if (p_eat_special(p, '{')) {
            d->patsyn.kind = PATSYN_RECORD;
            if (!p_at_special(p, '}')) {
                for (;;) {
                    Ident *f = p_varid(p);
                    if (f == NULL) {
                        return NULL;
                    }
                    vec_push(&d->patsyn.args, f->name);
                    if (!p_eat_special(p, ',')) {
                        break;
                    }
                }
            }
            if (p_expect_special(p, '}') != 0) {
                return NULL;
            }
        } else {
            d->patsyn.kind = PATSYN_PREFIX;
            while (p_kind(p) == TOK_VARID) {
                Ident *a;
                if (p_peek(p)->qual[0] != '\0') {
                    break;
                }
                a = p_varid(p);
                vec_push(&d->patsyn.args, a->name);
            }
        }
    } else {
        const Token *t;
        Ident *left = p_varid(p), *right;
        if (left == NULL) {
            return NULL;
        }
        d->patsyn.kind = PATSYN_INFIX;
        d->patsyn.left = left->name;
        t = p_peek(p);
        if (t->kind == TOK_CONSYM && t->qual[0] == '\0') {
            d->patsyn.name = xstrdup(t->text);
            p_bump(p);
        } else if (t->kind == TOK_RESERVEDOP && t->ch == OP_COLON) {
            p_bump(p);
            d->patsyn.name = xstrdup(":");
        } else if (tok_is_special(t, '`')) {
            Ident *c;
            p_bump(p);
            c = conid(p);
            if (c == NULL || p_expect_special(p, '`') != 0) {
                return NULL;
            }
            d->patsyn.name = str_printf("`%s`", c->name);
        } else {
            p_unexpected(p, "a constructor operator");
            return NULL;
        }
        right = p_varid(p);
        if (right == NULL) {
            return NULL;
        }
        d->patsyn.right = right->name;
    }
    if (p_eat_op(p, OP_EQUALS)) {
        d->dir = PATSYN_IMPLICIT;
        d->pat = p_pat(p);
        return d->pat ? d : NULL;
    }
    if (p_expect_op(p, OP_LEFTARROW, "`=` or `<-`") != 0) {
        return NULL;
    }
    d->pat = p_pat(p);
    if (d->pat == NULL) {
        return NULL;
    }
    if (p_eat_keyword(p, KW_WHERE)) {
        d->dir = PATSYN_EXPLICIT;
        if (p_local_decls(p, &d->body) != 0) {
            return NULL;
        }
    } else {
        d->dir = PATSYN_UNIDIRECTIONAL;
    }
    return d;
}

/* An operator as it appears in a fixity declaration: a symbol, or a name
 * in backquotes. */
static char *operator_name(Parser *p)
{
    const Token *t = p_peek(p);
    char *name;
    if ((t->kind == TOK_VARSYM || t->kind == TOK_CONSYM) && t->qual[0] == '\0') {
        name = xstrdup(t->text);
        p_bump(p);
        return name;
    }
    if (t->kind == TOK_RESERVEDOP && t->ch == OP_COLON) {
        p_bump(p);
        return xstrdup(":");
    }
    if (tok_is_special(t, '`')) {
        p_bump(p);
        t = p_peek(p);
        if (!((t->kind == TOK_VARID || t->kind == TOK_CONID) && t->qual[0] == '\0')) {
            p_unexpected(p, "a name in backquotes");
            return NULL;
        }
        name = str_printf("`%s`", t->text);
        p_bump(p);
        if (p_expect_special(p, '`') != 0) {
            return NULL;
        }
        return name;
    }
    p_unexpected(p, "an operator");
    return NULL;
}

static Decl *fixity(Parser *p)
{
    Decl *d = decl_new(DECL_FIXITY, p_pos(p));
    const Token *t;
    d->assoc = p_at_keyword(p, KW_INFIXL) ? ASSOC_LEFT
             : p_at_keyword(p, KW_INFIXR) ? ASSOC_RIGHT : ASSOC_NONE;
    p_bump(p);
    t = p_peek(p);
    if (t->kind == TOK_INTEGER) {
        d->prec = xstrdup(t->text);
        p_bump(p);
    }
    for (;;) {
        char *op = operator_name(p);
        if (op == NULL) {
            return NULL;
        }
        vec_push(&d->strs, op);
        if (!p_eat_special(p, ',')) {
            break;
        }
    }
    return d;
}

/* --- data / newtype ---------------------------------------------------- */

static int ends_constructor(const Token *t)
{
    switch (t->kind) {
    case TOK_RESERVEDOP:
        return t->ch == OP_BAR || t->ch == OP_EQUALS;
    case TOK_KEYWORD:
        return t->ch == KW_DERIVING || t->ch == KW_WHERE;
    case TOK_SPECIAL:
        return t->ch == ';';
    case TOK_VSEMI:
    case TOK_VCLOSE:
    case TOK_EOF:
        return 1;
    default:
        return 0;
    }
}

static int is_open(const Token *t)
{
    return tok_is_special(t, '(') || tok_is_special(t, '[') || tok_is_special(t, '{');
}

static int is_close(const Token *t)
{
    return tok_is_special(t, ')') || tok_is_special(t, ']') || tok_is_special(t, '}');
}

/* Whether a `=>` follows before the end of this constructor (or the
 * declaration head), at bracket depth zero. */
static int scan_for_double_arrow(const Parser *p)
{
    size_t i = p->idx;
    unsigned long depth = 0;
    const Token *t;
    while ((t = p_tok_at(p, i)) != NULL) {
        if (is_open(t)) {
            depth++;
        } else if (is_close(t)) {
            if (depth == 0) {
                return 0;
            }
            depth--;
        } else if (depth == 0 && t->kind == TOK_RESERVEDOP && t->ch == OP_DOUBLEARROW) {
            return 1;
        } else if (depth == 0 && ends_constructor(t)) {
            return 0;
        }
        i++;
    }
    return 0;
}

/* Whether a constructor operator follows before the end of this
 * constructor, at bracket depth zero. */
static int con_is_infix(const Parser *p)
{
    size_t i = p->idx;
    unsigned long depth = 0;
    const Token *t;
    while ((t = p_tok_at(p, i)) != NULL) {
        if (is_open(t)) {
            depth++;
        } else if (is_close(t)) {
            if (depth == 0) {
                return 0;
            }
            depth--;
        } else if (depth == 0 &&
                   (t->kind == TOK_CONSYM || (t->kind == TOK_RESERVEDOP && t->ch == OP_COLON))) {
            return 1;
        } else if (depth == 0 && tok_is_special(t, '`')) {
            const Token *u = p_tok_at(p, i + 1);
            return u != NULL && u->kind == TOK_CONID;
        } else if (depth == 0 && ends_constructor(t)) {
            return 0;
        }
        i++;
    }
    return 0;
}

/* An argument of an infix constructor: a type application such as
 * `List a`, or a strictness mark and an atype. */
static int infix_con_arg(Parser *p, BangType *out)
{
    if (p_at_varsym(p, "!") || p_at_op(p, OP_TILDE)) {
        return p_bang_type(p, 0, out);
    }
    out->strict = 0;
    out->ty = p_optype(p);
    return out->ty ? 0 : -1;
}

static int at_bang_type_start(const Parser *p)
{
    return p_at_atype_start(p) || p_at_varsym(p, "!") || p_at_op(p, OP_TILDE);
}

static int record_fields(Parser *p, Vec *fields)
{
    vec_init(fields);
    if (p_expect_special(p, '{') != 0) {
        return -1;
    }
    if (p_eat_special(p, '}')) {
        return 0;
    }
    for (;;) {
        Field *f = (Field *)xcalloc(1, sizeof(Field));
        for (;;) {
            Ident *name = sig_name(p);
            if (name == NULL) {
                return -1;
            }
            vec_push(&f->names, name);
            if (!p_eat_special(p, ',')) {
                break;
            }
        }
        if (p_expect_op(p, OP_DOUBLECOLON, "`::`") != 0 || p_bang_type(p, 1, &f->ty) != 0) {
            return -1;
        }
        vec_push(fields, f);
        if (!p_eat_special(p, ',')) {
            return p_expect_special(p, '}');
        }
    }
}

static ConDecl *con_decl(Parser *p)
{
    ConDecl *c = (ConDecl *)xcalloc(1, sizeof(ConDecl));
    c->pos = p_pos(p);
    if (p_at_varid(p, "forall")) {
        p_bump(p);
        if (p_ty_var_binds(p, &c->forall) != 0 || p_expect_varsym(p, ".") != 0) {
            return NULL;
        }
    }
    if (scan_for_double_arrow(p)) {
        c->ctx = p_btype(p);
        if (c->ctx == NULL || p_expect_op(p, OP_DOUBLEARROW, "`=>`") != 0) {
            return NULL;
        }
    }
    if (con_is_infix(p)) {
        Span span;
        const Token *t;
        if (infix_con_arg(p, &c->left) != 0) {
            return NULL;
        }
        span = p_token_span(p);
        t = p_peek(p);
        c->kind = CON_INFIX;
        if (t->kind == TOK_CONSYM && t->qual[0] == '\0') {
            c->name.name = xstrdup(t->text);
            c->name.span = span;
            p_bump(p);
        } else if (t->kind == TOK_RESERVEDOP && t->ch == OP_COLON) {
            p_bump(p);
            c->name.name = xstrdup(":");
            c->name.span = span;
        } else if (tok_is_special(t, '`')) {
            Ident *name;
            p_bump(p);
            name = conid(p);
            if (name == NULL || p_expect_special(p, '`') != 0) {
                return NULL;
            }
            c->name.name = str_printf("`%s`", name->name);
            c->name.span = name->span;
        } else {
            p_unexpected(p, "a constructor operator");
            return NULL;
        }
        if (infix_con_arg(p, &c->right) != 0) {
            return NULL;
        }
    } else if (p_at_special(p, '[') && tok_is_special(p_tok_at(p, p->idx + 1), ']')) {
        /* The empty list constructor, in the declaration of the list
         * type: `data List a = [] | a : List a`. */
        Pos start = p_pos(p);
        p->idx += 2;
        c->kind = CON_PREFIX;
        c->name.name = xstrdup("[]");
        c->name.span = p_span_from(p, start);
    } else {
        Ident *name = conid(p);
        if (name == NULL) {
            return NULL;
        }
        c->name = *name;
        if (p_at_special(p, '{')) {
            c->kind = CON_RECORD;
            if (record_fields(p, &c->fields) != 0) {
                return NULL;
            }
        } else {
            c->kind = CON_PREFIX;
            while (at_bang_type_start(p)) {
                BangType *b = (BangType *)xcalloc(1, sizeof(BangType));
                if (p_bang_type(p, 0, b) != 0) {
                    return NULL;
                }
                vec_push(&c->args, b);
            }
        }
    }
    return c;
}

static int deriving_clauses(Parser *p, Vec *clauses)
{
    vec_init(clauses);
    while (p_eat_keyword(p, KW_DERIVING)) {
        Deriving *dv = (Deriving *)xcalloc(1, sizeof(Deriving));
        if (p_eat_varid(p, "stock")) {
            dv->strategy = STRATEGY_STOCK;
        } else if (p_eat_varid(p, "anyclass")) {
            dv->strategy = STRATEGY_ANYCLASS;
        } else if (p_eat_keyword(p, KW_NEWTYPE)) {
            dv->strategy = STRATEGY_NEWTYPE;
        } else {
            dv->strategy = STRATEGY_NONE;
        }
        dv->parens = p_at_special(p, '(');
        if (dv->parens) {
            p_bump(p);
            if (!p_eat_special(p, ')')) {
                for (;;) {
                    Type *t = p_ty(p);
                    if (t == NULL) {
                        return -1;
                    }
                    vec_push(&dv->classes, t);
                    if (!p_eat_special(p, ',')) {
                        if (p_expect_special(p, ')') != 0) {
                            return -1;
                        }
                        break;
                    }
                }
            }
        } else {
            Type *t = p_atype(p);
            if (t == NULL) {
                return -1;
            }
            vec_push(&dv->classes, t);
        }
        if (p_eat_varid(p, "via")) {
            dv->via = p_atype(p);
            if (dv->via == NULL) {
                return -1;
            }
        }
        vec_push(clauses, dv);
    }
    return 0;
}

static Decl *data_decl(Parser *p, int newtype)
{
    Decl *d = decl_new(DECL_DATA, p_pos(p));
    QName *pname;
    d->newtype = newtype;
    p_bump(p);
    /* A type operator in prefix form: `data (:+:) f g p`. */
    pname = p_parenthesized_name(p);
    if (pname != NULL) {
        if (pname->qual[0] != '\0') {
            p_unexpected(p, "an unqualified type constructor");
            return NULL;
        }
        d->name.name = pname->name;
        d->name.span = pname->span;
    } else {
        Ident *name = conid(p);
        if (name == NULL) {
            return NULL;
        }
        d->name = *name;
    }
    if (p_ty_var_binds(p, &d->vars) != 0) {
        return NULL;
    }
    if (p_eat_op(p, OP_DOUBLECOLON)) {
        d->kind_sig = p_ty(p);
        if (d->kind_sig == NULL) {
            return NULL;
        }
    }
    if (p_eat_op(p, OP_EQUALS)) {
        for (;;) {
            ConDecl *c = con_decl(p);
            if (c == NULL) {
                return NULL;
            }
            vec_push(&d->cons, c);
            if (!p_eat_op(p, OP_BAR)) {
                break;
            }
        }
    }
    return deriving_clauses(p, &d->deriving) == 0 ? d : NULL;
}

/* --- type -------------------------------------------------------------- */

/* `type T a = t`, `type family F a :: k`, `type instance F Int = t`, and
 * the associated forms in class and instance bodies. */
static Decl *type_decl(Parser *p, DeclContext ctx)
{
    Pos pos = p_pos(p);
    int family, instance;
    Decl *d;
    Ident *name;
    p_bump(p);
    family = p_eat_varid(p, "family") || ctx == CTX_CLASS;
    instance = p_eat_keyword(p, KW_INSTANCE) || ctx == CTX_INSTANCE;
    if (instance) {
        d = decl_new(DECL_TYPE_INSTANCE, pos);
        d->lhs_ty = p_btype(p);
        if (d->lhs_ty == NULL || p_expect_op(p, OP_EQUALS, "`=`") != 0) {
            return NULL;
        }
        d->ty = p_ty(p);
        return d->ty ? d : NULL;
    }
    name = conid(p);
    if (name == NULL) {
        return NULL;
    }
    d = decl_new(family ? DECL_TYPE_FAMILY : DECL_TYPE_SYN, pos);
    d->name = *name;
    if (p_ty_var_binds(p, &d->vars) != 0) {
        return NULL;
    }
    if (family) {
        if (p_eat_op(p, OP_DOUBLECOLON)) {
            d->kind_sig = p_ty(p);
            if (d->kind_sig == NULL) {
                return NULL;
            }
        }
        return d;
    }
    if (p_expect_op(p, OP_EQUALS, "`=`") != 0) {
        return NULL;
    }
    d->ty = p_ty(p);
    return d->ty ? d : NULL;
}

/* --- class / instance -------------------------------------------------- */

static Decl *class_decl(Parser *p)
{
    Decl *d = decl_new(DECL_CLASS, p_pos(p));
    Ident *name;
    p_bump(p);
    if (scan_for_double_arrow(p)) {
        d->ctx = p_btype(p);
        if (d->ctx == NULL || p_expect_op(p, OP_DOUBLEARROW, "`=>`") != 0) {
            return NULL;
        }
    }
    name = conid(p);
    if (name == NULL) {
        return NULL;
    }
    d->name = *name;
    if (p_ty_var_binds(p, &d->vars) != 0) {
        return NULL;
    }
    return where_decls(p, CTX_CLASS, &d->body) == 0 ? d : NULL;
}

static Decl *instance_decl(Parser *p)
{
    Decl *d = decl_new(DECL_INSTANCE, p_pos(p));
    p_bump(p);
    d->ty = p_ty(p);
    if (d->ty == NULL) {
        return NULL;
    }
    return where_decls(p, CTX_INSTANCE, &d->body) == 0 ? d : NULL;
}

Decl *p_decl(Parser *p, DeclContext ctx)
{
    Pos pos = p_pos(p);
    const Token *t = p_peek(p);
    if (t->kind == TOK_KEYWORD) {
        switch (t->ch) {
        case KW_DATA:
            return data_decl(p, 0);
        case KW_NEWTYPE:
            return data_decl(p, 1);
        case KW_TYPE:
            return type_decl(p, ctx);
        case KW_CLASS:
            return class_decl(p);
        case KW_INSTANCE:
            return instance_decl(p);
        case KW_INFIX:
        case KW_INFIXL:
        case KW_INFIXR:
            return fixity(p);
        case KW_DERIVING:
            p_error(p, xstrdup("standalone deriving is not supported yet"));
            return NULL;
        case KW_FOREIGN:
            p_error(p, xstrdup("foreign declarations are not supported yet"));
            return NULL;
        case KW_DEFAULT:
            if (ctx == CTX_CLASS && next_is_varid_or_paren(p)) {
                Decl *d = decl_new(DECL_DEFAULT_SIG, pos);
                Ident *name;
                p_bump(p);
                name = sig_name(p);
                if (name == NULL || p_expect_op(p, OP_DOUBLECOLON, "`::`") != 0) {
                    return NULL;
                }
                d->name = *name;
                d->ty = p_ty(p);
                return d->ty ? d : NULL;
            }
            p_error(p, xstrdup("default declarations are not supported yet"));
            return NULL;
        default:
            break;
        }
    }
    if (ctx == CTX_TOP_LEVEL && p_at_varid(p, "pattern") && pattern_decl_ahead(p)) {
        return pat_syn(p);
    }
    return sig_or_binding(p);
}
