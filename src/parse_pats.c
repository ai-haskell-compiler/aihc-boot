/* Patterns.
 *
 *   pat  ::= lpat (qconop lpat)*                -- infix constructors
 *   lpat ::= apat | - literal | gcon apat+
 *   apat ::= var [@ apat] | gcon | qcon { fpats } | literal | _
 *          | ( pat ) | ( pat , pat ... ) | ( exp -> pat ) | ( pat :: type )
 *          | [ pat , ... ] | ~ apat | ! apat
 */

#include "parse.h"

#include <string.h>

static Pat *lpat(Parser *p);

/* A constructor operator, consumed: `:`, `:|`, `Con` in backquotes. NULL
 * when there is none. */
static QName *con_operator(Parser *p)
{
    Span span = p_token_span(p);
    const Token *t = p_peek(p);
    QName *op;
    if (t->kind == TOK_CONSYM) {
        op = qname_new(t->qual, t->text, span);
        p_bump(p);
        return op;
    }
    if (t->kind == TOK_RESERVEDOP && t->ch == OP_COLON) {
        p_bump(p);
        return qname_new("", ":", span);
    }
    if (tok_is_special(t, '`')) {
        const Token *inner = p_tok_at(p, p->idx + 1);
        Span s;
        if (inner == NULL || inner->kind != TOK_CONID) {
            return NULL;
        }
        if (!tok_is_special(p_tok_at(p, p->idx + 2), '`')) {
            return NULL;
        }
        s.start = inner->pos;
        s.end = inner->end;
        op = qname_new(inner->qual, inner->text, s);
        p->idx += 3;
        return op;
    }
    return NULL;
}

/* A pattern, with infix constructor operators. */
Pat *p_pat(Parser *p)
{
    Pat *first = lpat(p), *infix = NULL;
    QName *op;
    if (first == NULL) {
        return NULL;
    }
    while ((op = con_operator(p)) != NULL) {
        InfixStep *step = (InfixStep *)xcalloc(1, sizeof(InfixStep));
        step->op = *op;
        step->operand = lpat(p);
        if (step->operand == NULL) {
            return NULL;
        }
        if (infix == NULL) {
            infix = pat_new(PAT_INFIX);
            infix->a = first;
        }
        vec_push(&infix->steps, step);
    }
    return infix ? infix : first;
}

/* `C { f = p, g, .. }` after the constructor name. */
static Pat *record_pat(Parser *p, QName *name)
{
    Pat *r = pat_new(PAT_RECORD);
    r->con = *name;
    if (p_expect_special(p, '{') != 0) {
        return NULL;
    }
    if (!p_at_special(p, '}')) {
        for (;;) {
            FieldPat *f;
            QName *field;
            if (p_eat_op(p, OP_DOTDOT)) {
                r->wildcard = 1;
                break;
            }
            field = p_qname(p);
            if (field == NULL) {
                return NULL;
            }
            f = (FieldPat *)xcalloc(1, sizeof(FieldPat));
            f->name = *field;
            if (p_eat_op(p, OP_EQUALS)) {
                f->pat = p_pat(p);
                if (f->pat == NULL) {
                    return NULL;
                }
            }
            vec_push(&r->fields, f);
            if (!p_eat_special(p, ',')) {
                break;
            }
        }
    }
    if (p_expect_special(p, '}') != 0) {
        return NULL;
    }
    return r;
}

static Pat *con_pat(QName *name)
{
    Pat *c = pat_new(PAT_CON);
    c->con = *name;
    return c;
}

/* A pattern without infix operators at the top: a constructor
 * application, a negative literal, or an atomic pattern. */
static Pat *lpat(Parser *p)
{
    QName *name;
    if (p_at_varsym(p, "-")) {
        Pat *l = pat_new(PAT_LIT);
        p_bump(p);
        if (p_literal(p, &l->lit) != 0) {
            return NULL;
        }
        l->negative = 1;
        return l;
    }
    name = p_gcon(p);
    if (name != NULL) {
        Pat *c;
        if (p_at_special(p, '{')) {
            return record_pat(p, name);
        }
        c = con_pat(name);
        while (p_at_apat_start(p)) {
            Pat *arg = p_apat(p);
            if (arg == NULL) {
                return NULL;
            }
            vec_push(&c->args, arg);
        }
        return c;
    }
    return p_apat(p);
}

int p_literal(Parser *p, Literal *out)
{
    const Token *t = p_peek(p);
    memset(out, 0, sizeof *out);
    switch (t->kind) {
    case TOK_INTEGER:
        out->kind = LIT_INTEGER;
        break;
    case TOK_FLOAT:
        out->kind = LIT_FLOAT;
        break;
    case TOK_CHAR:
        out->kind = LIT_CHAR;
        out->ch = t->ch;
        break;
    case TOK_STRING:
        out->kind = LIT_STRING;
        out->value = t->value;
        out->value_len = t->value_len;
        break;
    default:
        return p_unexpected(p, "a literal");
    }
    out->raw = t->text;
    p_bump(p);
    return 0;
}

/* A constructor name at the current position, without consuming it:
 * `C`, `M.C`, `()`, `[]`, `(,)`, `(:|)`. NULL when there is none. */
static QName *gcon_ahead(const Parser *p)
{
    const Token *t = p_peek(p);
    Span none;
    memset(&none, 0, sizeof none);
    if (t->kind == TOK_CONID) {
        return qname_new(t->qual, t->text, none);
    }
    if (tok_is_special(t, '(')) {
        size_t i = p->idx + 1;
        const Token *u = p_tok_at(p, i);
        if (tok_is_special(u, ')')) {
            return qname_new("", "()", none);
        }
        if (tok_is_special(u, ',')) {
            StrBuf b;
            sb_init(&b);
            sb_putc(&b, '(');
            while (tok_is_special(p_tok_at(p, i), ',')) {
                sb_putc(&b, ',');
                i++;
            }
            if (tok_is_special(p_tok_at(p, i), ')')) {
                sb_putc(&b, ')');
                return qname_new("", b.s, none);
            }
            return NULL;
        }
        if (u != NULL && u->kind == TOK_CONSYM) {
            if (tok_is_special(p_tok_at(p, i + 1), ')')) {
                return qname_new(u->qual, u->text, none);
            }
            return NULL;
        }
        if (u != NULL && u->kind == TOK_RESERVEDOP && u->ch == OP_COLON) {
            if (tok_is_special(p_tok_at(p, i + 1), ')')) {
                return qname_new("", ":", none);
            }
            return NULL;
        }
        return NULL;
    }
    if (tok_is_special(t, '[')) {
        if (tok_is_special(p_tok_at(p, p->idx + 1), ']')) {
            return qname_new("", "[]", none);
        }
    }
    return NULL;
}

/* Consume the tokens of a constructor name that gcon_ahead found. */
static void bump_gcon(Parser *p, const QName *name)
{
    const Token *t = p_peek(p);
    if (t->kind == TOK_CONID) {
        p_bump(p);
    } else if (tok_is_special(t, '(')) {
        if (str_eq(name->name, "()") || str_starts_with(name->name, "(,")) {
            /* `(` `,`* `)` */
            p->idx += strlen(name->name);
        } else {
            /* `(` op `)` */
            p->idx += 3;
        }
    } else {
        /* `[]` */
        p->idx += 2;
    }
}

/* A constructor name, consumed, with its span: see gcon_ahead. */
QName *p_gcon(Parser *p)
{
    Pos start = p_pos(p);
    QName *name = gcon_ahead(p);
    if (name == NULL) {
        return NULL;
    }
    bump_gcon(p, name);
    name->span = p_span_from(p, start);
    return name;
}

int p_at_apat_start(const Parser *p)
{
    const Token *t = p_peek(p);
    switch (t->kind) {
    case TOK_VARID:
        return t->qual[0] == '\0';
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
    case TOK_RESERVEDOP:
        return t->ch == OP_TILDE;
    case TOK_VARSYM:
        return t->qual[0] == '\0' && str_eq(t->text, "!");
    default:
        return 0;
    }
}

/* Whether a `->` follows at bracket depth zero before the `,` or `)` that
 * ends this parenthesized item: the mark of a view pattern. */
static int view_arrow_ahead(const Parser *p)
{
    size_t i = p->idx;
    unsigned long depth = 0;
    const Token *t;
    while ((t = p_tok_at(p, i)) != NULL) {
        if (tok_is_special(t, '(') || tok_is_special(t, '[') || tok_is_special(t, '{') ||
            t->kind == TOK_VOPEN) {
            depth++;
        } else if (tok_is_special(t, ')') || tok_is_special(t, ']') ||
                   tok_is_special(t, '}') || t->kind == TOK_VCLOSE) {
            if (depth == 0) {
                return 0;
            }
            depth--;
        } else if (tok_is_special(t, ',') && depth == 0) {
            return 0;
        } else if (t->kind == TOK_RESERVEDOP && t->ch == OP_RIGHTARROW && depth == 0) {
            return 1;
        } else if (t->kind == TOK_EOF) {
            /* A lambda or case inside the item would need parentheses to
             * be an expression here, so `->` at depth zero is the view
             * arrow. */
            return 0;
        }
        i++;
    }
    return 0;
}

/* After `(`: a parenthesized pattern, a tuple, a view pattern or a
 * pattern with a type signature. */
static Pat *paren_pat(Parser *p)
{
    Vec items;
    Pat *r;
    vec_init(&items);
    if (p_expect_special(p, '(') != 0) {
        return NULL;
    }
    for (;;) {
        Pat *item;
        if (view_arrow_ahead(p)) {
            Expr *e = p_exp(p);
            if (e == NULL || p_expect_op(p, OP_RIGHTARROW, "`->`") != 0) {
                return NULL;
            }
            item = pat_new(PAT_VIEW);
            item->expr = e;
            item->a = p_pat(p);
            if (item->a == NULL) {
                return NULL;
            }
        } else {
            item = p_pat(p);
            if (item == NULL) {
                return NULL;
            }
            if (p_eat_op(p, OP_DOUBLECOLON)) {
                Pat *sig = pat_new(PAT_SIG);
                sig->a = item;
                sig->ty = p_ty(p);
                if (sig->ty == NULL) {
                    return NULL;
                }
                item = sig;
            }
        }
        vec_push(&items, item);
        if (!p_eat_special(p, ',')) {
            if (p_expect_special(p, ')') != 0) {
                return NULL;
            }
            break;
        }
    }
    if (items.len == 1) {
        r = pat_new(PAT_PAREN);
        r->a = (Pat *)items.items[0];
        return r;
    }
    r = pat_new(PAT_TUPLE);
    r->args = items;
    return r;
}

/* An atomic pattern. */
Pat *p_apat(Parser *p)
{
    const Token *t = p_peek(p);
    QName *name;
    Pat *r;
    switch (t->kind) {
    case TOK_VARID:
        if (t->qual[0] != '\0') {
            break;
        }
        r = pat_new(PAT_VAR);
        r->var.name = xstrdup(t->text);
        r->var.span = p_token_span(p);
        p_bump(p);
        if (p_eat_op(p, OP_AT)) {
            r->kind = PAT_AS;
            r->a = p_apat(p);
            if (r->a == NULL) {
                return NULL;
            }
        }
        return r;
    case TOK_KEYWORD:
        if (t->ch != KW_UNDERSCORE) {
            break;
        }
        p_bump(p);
        return pat_new(PAT_WILDCARD);
    case TOK_RESERVEDOP:
        if (t->ch != OP_TILDE) {
            break;
        }
        p_bump(p);
        r = pat_new(PAT_LAZY);
        r->a = p_apat(p);
        return r->a ? r : NULL;
    case TOK_VARSYM:
        if (!p_at_varsym(p, "!")) {
            break;
        }
        p_bump(p);
        r = pat_new(PAT_BANG);
        r->a = p_apat(p);
        return r->a ? r : NULL;
    case TOK_INTEGER:
    case TOK_FLOAT:
    case TOK_CHAR:
    case TOK_STRING:
        r = pat_new(PAT_LIT);
        if (p_literal(p, &r->lit) != 0) {
            return NULL;
        }
        return r;
    case TOK_SPECIAL:
        if (t->ch == '[') {
            name = p_gcon(p);
            if (name != NULL) {
                return con_pat(name);
            }
            p_bump(p);
            r = pat_new(PAT_LIST);
            for (;;) {
                Pat *item = p_pat(p);
                if (item == NULL) {
                    return NULL;
                }
                vec_push(&r->args, item);
                if (!p_eat_special(p, ',')) {
                    if (p_expect_special(p, ']') != 0) {
                        return NULL;
                    }
                    return r;
                }
            }
        }
        if (t->ch == '(') {
            name = p_gcon(p);
            if (name != NULL) {
                if (p_at_special(p, '{')) {
                    return record_pat(p, name);
                }
                return con_pat(name);
            }
            return paren_pat(p);
        }
        break;
    case TOK_CONID:
        name = p_gcon(p);
        if (p_at_special(p, '{')) {
            return record_pat(p, name);
        }
        return con_pat(name);
    default:
        break;
    }
    p_unexpected(p, "a pattern");
    return NULL;
}
