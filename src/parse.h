/* The shared state and helpers of the parser. Only the parse*.c files
 * include this header.
 *
 * Conventions: a function that returns a pointer returns NULL on a
 * parse error, and a function that returns int returns -1. In both cases
 * the error is in p->err. Functions whose names end in `_ahead` or that
 * return an "optional" result do not set an error: NULL means "not
 * here".
 */
#ifndef AIHC_PARSE_H
#define AIHC_PARSE_H

#include "syntax.h"

typedef struct {
    TokenVec *toks;
    size_t idx;
    SyntaxError err;
} Parser;

/* Where a declaration appears. Class bodies permit `default`
 * signatures. */
typedef enum {
    CTX_TOP_LEVEL,
    CTX_CLASS,
    CTX_INSTANCE,
    CTX_LOCAL /* a `let` or `where` block */
} DeclContext;

/* --- Token access (parse.c) --- */

const Token *p_peek(const Parser *p);
/* The token at index i, or NULL past the end. */
const Token *p_tok_at(const Parser *p, size_t i);
TokenKind p_kind(const Parser *p);
Pos p_pos(const Parser *p);
Span p_token_span(const Parser *p);
Span p_span_from(const Parser *p, Pos start);
void p_bump(Parser *p);
int p_at_keyword(const Parser *p, Keyword k);
int p_at_special(const Parser *p, int c);
int p_at_op(const Parser *p, ReservedOp op);
int p_at_varid(const Parser *p, const char *name);
int p_at_varsym(const Parser *p, const char *sym);
int p_eat_keyword(Parser *p, Keyword k);
int p_eat_special(Parser *p, int c);
int p_eat_op(Parser *p, ReservedOp op);
int p_eat_varid(Parser *p, const char *name);
int p_eat_kind(Parser *p, TokenKind kind);
/* Token i is a special character c. */
int tok_is_special(const Token *t, int c);

int p_error(Parser *p, char *message);
int p_unexpected(Parser *p, const char *expected);
int p_expect_op(Parser *p, ReservedOp op, const char *what);
int p_expect_keyword(Parser *p, Keyword k);
int p_expect_special(Parser *p, int c);
int p_expect_varsym(Parser *p, const char *sym);

/* --- Blocks and names (parse.c) --- */

/* A block of items. The item function returns NULL on error. Returns -1
 * on error and fills *items (void *) on success. */
typedef void *(*ItemFn)(Parser *p, void *env);
int p_block(Parser *p, ItemFn item, void *env, Vec *items);
QName *p_parenthesized_name(Parser *p);
QName *p_qname(Parser *p);

/* --- Declarations (parse_decls.c) --- */

Decl *p_decl(Parser *p, DeclContext ctx);
/* Fills *decls. */
int p_local_decls(Parser *p, Vec *decls);

/* --- Expressions (parse_exprs.c) --- */

Expr *p_exp(Parser *p);
Stmt *p_stmt(Parser *p);
Rhs *p_rhs(Parser *p, ReservedOp sep);
/* An operator at the current position, not consumed. Returns NULL when
 * there is none, and sets *len to its number of tokens. */
QName *p_operator_ahead(const Parser *p, size_t *len);

/* --- Patterns (parse_pats.c) --- */

Pat *p_pat(Parser *p);
Pat *p_apat(Parser *p);
int p_at_apat_start(const Parser *p);
int p_literal(Parser *p, Literal *out);
QName *p_gcon(Parser *p);

/* --- Types (parse_types.c) --- */

Type *p_ty(Parser *p);
Type *p_btype(Parser *p);
Type *p_optype(Parser *p);
Type *p_atype(Parser *p);
int p_at_atype_start(const Parser *p);
/* Fills *binders (TyVarBind *). */
int p_ty_var_binds(Parser *p, Vec *binders);
Ident *p_varid(Parser *p);
/* full: a full type after `::`; otherwise an atype. */
int p_bang_type(Parser *p, int full, BangType *out);

#endif
