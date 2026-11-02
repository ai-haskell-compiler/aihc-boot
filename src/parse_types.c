/* Types and kinds.
 *
 * The grammar, from loose to tight:
 *
 *   type   ::= forall tvs . type
 *            | btype => type
 *            | btype -> type
 *            | btype
 *   btype  ::= optype (typeop optype)*        -- infix type operators
 *   optype ::= atype+                         -- application, with @k
 *   atype  ::= tyvar | qtycon | ( ) | ( -> ) | (,) | [ ]
 *            | ( type ) | ( type , type ... ) | ( type :: kind )
 *            | [ type ] | ' atype | _ | literal
 *
 * Type operators stay in source order. The tree keeps every parenthesis,
 * so the printer reproduces the tokens and a reader sees the same
 * fixity.
 */

#include "parse.h"

#include <string.h>

static Type *binary(TypeKind kind, Type *a, Type *b)
{
    Type *t = type_new(kind);
    t->a = a;
    t->b = b;
    return t;
}

/* A type with optional `forall` and context. */
Type *p_ty(Parser *p)
{
    Type *left, *right;
    if (p_at_varid(p, "forall")) {
        Type *t = type_new(TY_FORALL);
        p_bump(p);
        if (p_ty_var_binds(p, &t->binders) != 0 || p_expect_varsym(p, ".") != 0) {
            return NULL;
        }
        t->a = p_ty(p);
        return t->a ? t : NULL;
    }
    left = p_btype(p);
    if (left == NULL) {
        return NULL;
    }
    if (p_eat_op(p, OP_DOUBLEARROW)) {
        right = p_ty(p);
        return right ? binary(TY_QUAL, left, right) : NULL;
    }
    if (p_eat_op(p, OP_RIGHTARROW)) {
        right = p_ty(p);
        return right ? binary(TY_FUN, left, right) : NULL;
    }
    return left;
}

static int next_is_varid(const Parser *p)
{
    const Token *t = p_tok_at(p, p->idx + 1);
    return t != NULL && t->kind == TOK_VARID && t->qual[0] == '\0';
}

/* Type variable binders up to the `.` of a `forall`, or the end of a
 * declaration head: `a`, `(a :: k)`. */
int p_ty_var_binds(Parser *p, Vec *binders)
{
    vec_init(binders);
    for (;;) {
        const Token *t = p_peek(p);
        TyVarBind *b;
        if (t->kind == TOK_VARID && t->qual[0] == '\0') {
            b = (TyVarBind *)xcalloc(1, sizeof(TyVarBind));
            b->name.name = xstrdup(t->text);
            b->name.span = p_token_span(p);
            vec_push(binders, b);
            p_bump(p);
        } else if (tok_is_special(t, '(') && next_is_varid(p)) {
            Ident *name;
            p_bump(p);
            name = p_varid(p);
            if (name == NULL) {
                return -1;
            }
            if (p_expect_op(p, OP_DOUBLECOLON, "`::` in a kinded binder") != 0) {
                return -1;
            }
            b = (TyVarBind *)xcalloc(1, sizeof(TyVarBind));
            b->name = *name;
            b->kind = p_ty(p);
            if (b->kind == NULL || p_expect_special(p, ')') != 0) {
                return -1;
            }
            vec_push(binders, b);
        } else {
            return 0;
        }
    }
}

/* An unqualified variable name. */
Ident *p_varid(Parser *p)
{
    const Token *t = p_peek(p);
    Ident *name;
    if (t->kind == TOK_VARID && t->qual[0] == '\0') {
        name = ident_new(t->text, p_token_span(p));
        p_bump(p);
        return name;
    }
    p_unexpected(p, "a variable");
    return NULL;
}

/* A type operator, consumed: `:+:`, `~`, `Op` in backquotes or a symbol
 * such as `+`. NULL when there is none. */
static QName *type_operator(Parser *p)
{
    Span span = p_token_span(p);
    const Token *t = p_peek(p);
    QName *op;
    if (t->kind == TOK_CONSYM || t->kind == TOK_VARSYM) {
        op = qname_new(t->qual, t->text, span);
    } else if (t->kind == TOK_RESERVEDOP && t->ch == OP_TILDE) {
        op = qname_new("", "~", span);
    } else if (tok_is_special(t, '`')) {
        const Token *inner = p_tok_at(p, p->idx + 1);
        Span s;
        if (inner == NULL || (inner->kind != TOK_CONID && inner->kind != TOK_VARID)) {
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
    } else {
        return NULL;
    }
    p_bump(p);
    return op;
}

/* Applications joined by infix type operators. */
Type *p_btype(Parser *p)
{
    Type *left = p_optype(p);
    QName *op;
    if (left == NULL) {
        return NULL;
    }
    while ((op = type_operator(p)) != NULL) {
        Type *right = p_optype(p);
        Type *t;
        if (right == NULL) {
            return NULL;
        }
        t = binary(TY_OP, left, right);
        t->con = *op;
        left = t;
    }
    return left;
}

/* One or more atypes applied to each other. */
Type *p_optype(Parser *p)
{
    Type *ty = p_atype(p);
    if (ty == NULL) {
        return NULL;
    }
    for (;;) {
        if (p_at_op(p, OP_AT)) {
            Type *kind;
            p_bump(p);
            kind = p_atype(p);
            if (kind == NULL) {
                return NULL;
            }
            ty = binary(TY_APP_KIND, ty, kind);
        } else if (p_at_atype_start(p)) {
            Type *arg = p_atype(p);
            if (arg == NULL) {
                return NULL;
            }
            ty = binary(TY_APP, ty, arg);
        } else {
            return ty;
        }
    }
}

int p_at_atype_start(const Parser *p)
{
    const Token *t = p_peek(p);
    switch (t->kind) {
    case TOK_VARID:
        /* `forall` starts a nested type, never an argument. */
        return !(t->qual[0] == '\0' && str_eq(t->text, "forall"));
    case TOK_CONID:
    case TOK_TICK:
    case TOK_STRING:
    case TOK_INTEGER:
    case TOK_CHAR:
        return 1;
    case TOK_SPECIAL:
        return t->ch == '(' || t->ch == '[';
    case TOK_KEYWORD:
        return t->ch == KW_UNDERSCORE;
    default:
        return 0;
    }
}

/* `[ ]`, `[ t ]` or `[ t, t ]`. */
static int bracketed_types(Parser *p, Vec *items)
{
    vec_init(items);
    if (p_expect_special(p, '[') != 0) {
        return -1;
    }
    if (p_eat_special(p, ']')) {
        return 0;
    }
    for (;;) {
        Type *t = p_ty(p);
        if (t == NULL) {
            return -1;
        }
        vec_push(items, t);
        if (!p_eat_special(p, ',')) {
            return p_expect_special(p, ']');
        }
    }
}

static Type *con(const char *name, Span span)
{
    Type *t = type_new(TY_CON);
    t->con.qual = xstrdup("");
    t->con.name = xstrdup(name);
    t->con.span = span;
    return t;
}

/* After a consumed `(`: an operator followed by `)`. The caller sets the
 * span, because it knows where `(` is. */
static QName *parenthesized_name_after_open(Parser *p)
{
    const Token *t = p_peek(p);
    QName *name;
    Span none;
    memset(&none, 0, sizeof none);
    if (t->kind == TOK_VARSYM || t->kind == TOK_CONSYM) {
        name = qname_new(t->qual, t->text, none);
    } else if (t->kind == TOK_RESERVEDOP && t->ch == OP_TILDE) {
        name = qname_new("", "~", none);
    } else {
        return NULL;
    }
    if (!tok_is_special(p_tok_at(p, p->idx + 1), ')')) {
        return NULL;
    }
    p->idx += 2;
    return name;
}

/* Everything that starts with `(`: unit, tuple constructors, the
 * function arrow, parenthesized types, tuples and kind signatures. */
static Type *paren_type(Parser *p)
{
    Pos start = p_pos(p);
    QName *name;
    Type *first, *t;
    if (p_expect_special(p, '(') != 0) {
        return NULL;
    }
    if (p_eat_special(p, ')')) {
        return con("()", p_span_from(p, start));
    }
    if (p_at_op(p, OP_RIGHTARROW)) {
        p_bump(p);
        if (p_expect_special(p, ')') != 0) {
            return NULL;
        }
        return con("->", p_span_from(p, start));
    }
    if (p_at_special(p, ',')) {
        StrBuf b;
        sb_init(&b);
        sb_putc(&b, '(');
        while (p_eat_special(p, ',')) {
            sb_putc(&b, ',');
        }
        sb_putc(&b, ')');
        if (p_expect_special(p, ')') != 0) {
            return NULL;
        }
        return con(b.s, p_span_from(p, start));
    }
    /* An operator in parentheses, such as `(:+:)` or `(~)`. */
    name = parenthesized_name_after_open(p);
    if (name != NULL) {
        t = type_new(TY_CON);
        t->con = *name;
        t->con.span = p_span_from(p, start);
        return t;
    }
    first = p_ty(p);
    if (first == NULL) {
        return NULL;
    }
    if (p_eat_op(p, OP_DOUBLECOLON)) {
        Type *kind = p_ty(p);
        if (kind == NULL || p_expect_special(p, ')') != 0) {
            return NULL;
        }
        t = type_new(TY_PAREN);
        t->a = binary(TY_KIND_SIG, first, kind);
        t->span = p_span_from(p, start);
        return t;
    }
    if (p_eat_special(p, ')')) {
        t = type_new(TY_PAREN);
        t->a = first;
        t->span = p_span_from(p, start);
        return t;
    }
    t = type_new(TY_TUPLE);
    vec_push(&t->items, first);
    while (p_eat_special(p, ',')) {
        Type *item = p_ty(p);
        if (item == NULL) {
            return NULL;
        }
        vec_push(&t->items, item);
    }
    if (p_expect_special(p, ')') != 0) {
        return NULL;
    }
    t->span = p_span_from(p, start);
    return t;
}

/* An atomic type. */
Type *p_atype(Parser *p)
{
    const Token *tok = p_peek(p);
    Type *t;
    switch (tok->kind) {
    case TOK_VARID:
        if (tok->qual[0] != '\0') {
            break;
        }
        t = type_new(TY_VAR);
        t->var.name = xstrdup(tok->text);
        t->var.span = p_token_span(p);
        p_bump(p);
        return t;
    case TOK_CONID:
        t = type_new(TY_CON);
        t->con.qual = xstrdup(tok->qual);
        t->con.name = xstrdup(tok->text);
        t->con.span = p_token_span(p);
        p_bump(p);
        return t;
    case TOK_KEYWORD:
        if (tok->ch != KW_UNDERSCORE) {
            break;
        }
        p_bump(p);
        return type_new(TY_WILDCARD);
    case TOK_TICK:
        p_bump(p);
        if (p_at_special(p, '[')) {
            t = type_new(TY_PROMOTED_LIST);
            if (bracketed_types(p, &t->items) != 0) {
                return NULL;
            }
            return t;
        }
        t = type_new(TY_PROMOTED);
        t->a = p_atype(p);
        return t->a ? t : NULL;
    case TOK_STRING:
    case TOK_INTEGER:
    case TOK_CHAR:
        t = type_new(TY_LIT);
        if (p_literal(p, &t->lit) != 0) {
            return NULL;
        }
        return t;
    case TOK_SPECIAL:
        if (tok->ch == '[') {
            Pos start = p_pos(p);
            Vec items;
            Span span;
            if (bracketed_types(p, &items) != 0) {
                return NULL;
            }
            span = p_span_from(p, start);
            if (items.len == 0) {
                return con("[]", span);
            }
            if (items.len == 1) {
                t = type_new(TY_LIST);
                t->a = (Type *)items.items[0];
                t->span = span;
                return t;
            }
            t = type_new(TY_PROMOTED_LIST);
            t->items = items;
            return t;
        }
        if (tok->ch == '(') {
            return paren_type(p);
        }
        break;
    default:
        break;
    }
    p_unexpected(p, "a type");
    return NULL;
}

/* A constructor argument or field type: optional `!` or `~`, then an
 * atype (or a full type after `::`, which the caller chooses with
 * full). */
int p_bang_type(Parser *p, int full, BangType *out)
{
    out->strict = 0;
    if (p_at_varsym(p, "!")) {
        p_bump(p);
        out->strict = 1;
    } else if (p_eat_op(p, OP_TILDE)) {
        out->strict = 2;
    }
    out->ty = full ? p_ty(p) : p_atype(p);
    return out->ty ? 0 : -1;
}
