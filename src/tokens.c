/* Tokens, positions and the constructors of the syntax tree. */

#include "syntax.h"

#include <string.h>

int pos_cmp(Pos a, Pos b)
{
    if (a.line != b.line) {
        return a.line < b.line ? -1 : 1;
    }
    if (a.col != b.col) {
        return a.col < b.col ? -1 : 1;
    }
    return 0;
}

static const char *const keywords[] = {
    "case", "class", "data", "default", "deriving", "do", "else", "foreign",
    "if", "import", "in", "infix", "infixl", "infixr", "instance", "let",
    "module", "newtype", "of", "then", "type", "where", "_"
};

static const char *const reserved_ops[] = {
    "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"
};

int keyword_parse(const char *s)
{
    size_t i;
    for (i = 0; i < sizeof keywords / sizeof keywords[0]; i++) {
        if (strcmp(s, keywords[i]) == 0) {
            return (int)i;
        }
    }
    return -1;
}

const char *keyword_str(int k)
{
    return keywords[k];
}

int reservedop_parse(const char *s)
{
    size_t i;
    for (i = 0; i < sizeof reserved_ops / sizeof reserved_ops[0]; i++) {
        if (strcmp(s, reserved_ops[i]) == 0) {
            return (int)i;
        }
    }
    return -1;
}

const char *reservedop_str(int op)
{
    return reserved_ops[op];
}

char *token_str(const Token *t)
{
    switch (t->kind) {
    case TOK_VARID:
    case TOK_CONID:
    case TOK_VARSYM:
    case TOK_CONSYM:
        if (t->qual[0] == '\0') {
            return xstrdup(t->text);
        }
        return str_concat3(t->qual, ".", t->text);
    case TOK_INTEGER:
    case TOK_FLOAT:
    case TOK_CHAR:
    case TOK_STRING:
        return xstrdup(t->text);
    case TOK_KEYWORD:
        return xstrdup(keyword_str((int)t->ch));
    case TOK_RESERVEDOP:
        return xstrdup(reservedop_str((int)t->ch));
    case TOK_SPECIAL:
        if (t->ch == SPECIAL_UOPEN) {
            return xstrdup("(#");
        }
        if (t->ch == SPECIAL_UCLOSE) {
            return xstrdup("#)");
        }
        return str_printf("%c", (int)t->ch);
    case TOK_PRAGMA:
        return str_printf("{-#%s#-}", t->text);
    case TOK_TICK:
        return xstrdup("'");
    case TOK_VOPEN:
        return xstrdup("{(virtual)");
    case TOK_VCLOSE:
        return xstrdup("}(virtual)");
    case TOK_VSEMI:
        return xstrdup(";(virtual)");
    case TOK_EOF:
        return xstrdup("<end of input>");
    }
    return xstrdup("");
}

void tokvec_push(TokenVec *v, Token t)
{
    if (v->len == v->cap) {
        v->cap = v->cap ? v->cap * 2 : 256;
        v->items = (Token *)xrealloc(v->items, v->cap * sizeof(Token));
    }
    v->items[v->len++] = t;
}

QName *qname_new(const char *qual, const char *name, Span span)
{
    QName *q = (QName *)xcalloc(1, sizeof(QName));
    q->qual = xstrdup(qual);
    q->name = xstrdup(name);
    q->span = span;
    return q;
}

Ident *ident_new(const char *name, Span span)
{
    Ident *i = (Ident *)xcalloc(1, sizeof(Ident));
    i->name = xstrdup(name);
    i->span = span;
    return i;
}

/* The nodes start zeroed: empty vectors, NULL pointers, and zero
 * positions. Strings that a kind does not use stay NULL. */

Type *type_new(TypeKind kind)
{
    Type *t = (Type *)xcalloc(1, sizeof(Type));
    t->kind = kind;
    return t;
}

Pat *pat_new(PatKind kind)
{
    Pat *p = (Pat *)xcalloc(1, sizeof(Pat));
    p->kind = kind;
    return p;
}

Expr *expr_new(ExprKind kind)
{
    Expr *e = (Expr *)xcalloc(1, sizeof(Expr));
    e->kind = kind;
    return e;
}

Decl *decl_new(DeclKind kind, Pos pos)
{
    Decl *d = (Decl *)xcalloc(1, sizeof(Decl));
    d->kind = kind;
    d->pos = pos;
    return d;
}
