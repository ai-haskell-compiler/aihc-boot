/* Fixity resolution, as section 10.6 of the Haskell 2010 report
 * specifies. The typechecker and the desugarer both use it, so they see
 * the same tree for an operator chain.
 */
#ifndef AIHC_FIXITY_H
#define AIHC_FIXITY_H

#include "strmap.h"
#include "syntax.h"

typedef struct {
    int prec;
    Assoc assoc;
} Fixity;

/* infixl 9 */
Fixity fixity_default(void);

/* Add the fixity declarations of a module to a table. The keys are
 * "MODULE\tNAME" and the values are Fixity *. */
void fixity_collect(StrMap *table, const char *module, const Module *m);

typedef enum { FT_EXP, FT_OP, FT_NEG } FTokKind;

/* One token of a chain: an operand, an operator with its fixity, or a
 * prefix minus. For FT_NEG, exp is the EXPR_NEG expression. */
typedef struct {
    FTokKind kind;
    const void *exp;
    const QName *op;
    Fixity fix;
} FTok;

typedef enum { FN_LEAF, FN_BIN, FN_NEG } FNodeKind;

/* The resolved tree. FN_LEAF: leaf is the operand. FN_BIN: op, l, r.
 * FN_NEG: l is the operand, and leaf is the EXPR_NEG expression. */
typedef struct FNode FNode;
struct FNode {
    FNodeKind kind;
    const void *leaf;
    const QName *op;
    FNode *l;
    FNode *r;
};

/* Resolve the tokens (FTok *). Returns NULL and sets *err when the chain
 * mixes operators that do not combine. */
FNode *fixity_resolve(const Vec *toks, char **err);

void ftok_push(Vec *toks, FTokKind kind, const void *exp, const QName *op, Fixity fix);

#endif
