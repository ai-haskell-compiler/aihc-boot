/* Fixity resolution. See fixity.h. */
#include <stdlib.h>

#include "fixity.h"
#include "resolve.h"

Fixity fixity_default(void)
{
    Fixity f;
    f.prec = 9;
    f.assoc = ASSOC_LEFT;
    return f;
}

static void collect_decls(StrMap *table, const char *module, const Vec *decls)
{
    size_t i;
    size_t j;
    for (i = 0; i < decls->len; i++) {
        const Decl *d = (const Decl *)decls->items[i];
        if (d->kind == DECL_CLASS) {
            collect_decls(table, module, &d->body);
            continue;
        }
        if (d->kind != DECL_FIXITY) {
            continue;
        }
        for (j = 0; j < d->strs.len; j++) {
            Fixity *f = (Fixity *)xmalloc(sizeof *f);
            f->assoc = d->assoc;
            f->prec = d->prec != NULL ? atoi(d->prec) : 9;
            strmap_put(table, str_printf("%s\t%s", module, bare((const char *)d->strs.items[j])),
                       f);
        }
    }
}

void fixity_collect(StrMap *table, const char *module, const Module *m)
{
    collect_decls(table, module, &m->decls);
}

void ftok_push(Vec *toks, FTokKind kind, const void *exp, const QName *op, Fixity fix)
{
    FTok *t = (FTok *)xcalloc(1, sizeof *t);
    t->kind = kind;
    t->exp = exp;
    t->op = op;
    t->fix = fix;
    vec_push(toks, t);
}

typedef struct {
    const Vec *toks;
    size_t pos;
    char *err;
} FixParser;

static FNode *fnode(FNodeKind kind)
{
    FNode *n = (FNode *)xcalloc(1, sizeof *n);
    n->kind = kind;
    return n;
}

static const FTok *tok_at(const FixParser *p, size_t i)
{
    return (const FTok *)p->toks->items[i];
}

static FNode *fix_parse(FixParser *p, Fixity op1);

static FNode *fix_parse1(FixParser *p, Fixity op1, FNode *e1)
{
    for (;;) {
        const FTok *t;
        Fixity op2;
        FNode *r;
        FNode *bin;
        if (p->err != NULL || p->pos >= p->toks->len) {
            return e1;
        }
        t = tok_at(p, p->pos);
        op2 = t->fix;
        if (op1.prec == op2.prec && (op1.assoc != op2.assoc || op1.assoc == ASSOC_NONE)) {
            p->err = str_printf("cannot mix operators of the same precedence: %s", t->op->name);
            return e1;
        }
        if (op1.prec > op2.prec || (op1.prec == op2.prec && op1.assoc == ASSOC_LEFT)) {
            return e1;
        }
        p->pos++;
        r = fix_parse(p, op2);
        bin = fnode(FN_BIN);
        bin->op = t->op;
        bin->l = e1;
        bin->r = r;
        e1 = bin;
    }
}

static FNode *fix_parse(FixParser *p, Fixity op1)
{
    const FTok *t = tok_at(p, p->pos);
    FNode *e1;
    if (t->kind == FT_NEG) {
        Fixity neg;
        FNode *n;
        if (op1.prec >= 6) {
            p->err = xstrdup("cannot mix prefix negation with an operator of precedence 6 or "
                             "more");
            return fnode(FN_LEAF);
        }
        p->pos++;
        neg.prec = 6;
        neg.assoc = ASSOC_LEFT;
        n = fnode(FN_NEG);
        n->leaf = t->exp;
        n->l = fix_parse(p, neg);
        return fix_parse1(p, op1, n);
    }
    p->pos++;
    e1 = fnode(FN_LEAF);
    e1->leaf = t->exp;
    return fix_parse1(p, op1, e1);
}

FNode *fixity_resolve(const Vec *toks, char **err)
{
    FixParser p;
    Fixity start;
    FNode *n;
    p.toks = toks;
    p.pos = 0;
    p.err = NULL;
    start.prec = -1;
    start.assoc = ASSOC_NONE;
    n = fix_parse(&p, start);
    if (p.err != NULL) {
        *err = p.err;
        return NULL;
    }
    return n;
}
