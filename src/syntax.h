/* Syntax of the Haskell subset that aihc and its vendored dependencies
 * use.
 *
 * The syntax code has three stages. Each stage takes the output of the
 * one before it:
 *
 * 1. lexer.c: source text to tokens.
 * 2. layout.c: virtual braces and semicolons from indentation.
 * 3. parse*.c: tokens to a syntax tree.
 *
 * print.c goes the other way, from a syntax tree to source text. The
 * boot compiler uses it to check the parser against aihc-parser, the
 * parser aihc itself uses: a module counts as parsed only when
 * aihc-parser reads the printed source as the same module.
 *
 * Conventions: every string is a fresh NUL-terminated UTF-8 string. A
 * qualifier is "" for an unqualified name. A Vec holds pointers; the
 * comment on each field names the type of its items. A pointer field
 * that is "optional" is NULL when absent.
 */
#ifndef AIHC_SYNTAX_H
#define AIHC_SYNTAX_H

#include "util.h"

/* --- Positions and tokens ---------------------------------------------- */

/* A position in a source file. Lines and columns start at 1. A tab moves
 * the column to the next multiple of 8 plus 1, as the Haskell report
 * specifies for the layout rule. */
typedef struct {
    unsigned long line;
    unsigned long col;
} Pos;

/* -1, 0 or 1. */
int pos_cmp(Pos a, Pos b);

/* Where a piece of syntax is in the source: the position of its first
 * character, and the position after its last character. Spans do not
 * take part in the comparison of two trees. */
typedef struct {
    Pos start;
    Pos end;
} Span;

typedef enum {
    TOK_VARID,    /* qual, text: `map`, `M.map` */
    TOK_CONID,    /* qual, text: `Just`, `M.Just` */
    TOK_VARSYM,   /* qual, text: `+`, `M.+` */
    TOK_CONSYM,   /* qual, text: `:|`, `M.:|` */
    TOK_INTEGER,  /* text: the source spelling */
    TOK_FLOAT,    /* text: the source spelling */
    TOK_CHAR,     /* ch: the value; text: the source spelling */
    TOK_STRING,   /* value, value_len: the decoded value; text: the source spelling */
    TOK_KEYWORD,  /* ch: a Keyword */
    TOK_RESERVEDOP, /* ch: a ReservedOp */
    TOK_SPECIAL,  /* ch: one of ( ) , ; [ ] ` { } */
    TOK_PRAGMA,   /* text: a LANGUAGE pragma without `{-#` and `#-}` */
    TOK_TICK,     /* a lone `'` (DataKinds) */
    TOK_VOPEN,    /* a virtual `{` */
    TOK_VCLOSE,   /* a virtual `}` */
    TOK_VSEMI,    /* a virtual `;` */
    TOK_EOF
} TokenKind;

/* Reserved words. `as`, `hiding` and `qualified` are not reserved in
 * Haskell. The lexer returns them as TOK_VARID; the parser recognizes
 * them by name. */
typedef enum {
    KW_CASE,
    KW_CLASS,
    KW_DATA,
    KW_DEFAULT,
    KW_DERIVING,
    KW_DO,
    KW_ELSE,
    KW_FOREIGN,
    KW_IF,
    KW_IMPORT,
    KW_IN,
    KW_INFIX,
    KW_INFIXL,
    KW_INFIXR,
    KW_INSTANCE,
    KW_LET,
    KW_MODULE,
    KW_NEWTYPE,
    KW_OF,
    KW_THEN,
    KW_TYPE,
    KW_WHERE,
    KW_UNDERSCORE /* the wildcard `_` */
} Keyword;

typedef enum {
    OP_DOTDOT,      /* .. */
    OP_COLON,       /* : */
    OP_DOUBLECOLON, /* :: */
    OP_EQUALS,      /* = */
    OP_BACKSLASH,   /* \ */
    OP_BAR,         /* | */
    OP_LEFTARROW,   /* <- */
    OP_RIGHTARROW,  /* -> */
    OP_AT,          /* @ */
    OP_TILDE,       /* ~ */
    OP_DOUBLEARROW  /* => */
} ReservedOp;

/* -1 when s is not a keyword or reserved operator. */
int keyword_parse(const char *s);
const char *keyword_str(int k);
int reservedop_parse(const char *s);
const char *reservedop_str(int op);

/* A token with the position of its first character, and the position
 * after its last character. A virtual token of the layout rule has no
 * characters: its end is its position. */
typedef struct {
    TokenKind kind;
    char *qual;          /* names only; "" otherwise */
    char *text;          /* see TokenKind; "" when unused */
    char *value;         /* TOK_STRING only */
    size_t value_len;    /* TOK_STRING only; the value may hold NUL */
    unsigned long ch;    /* TOK_CHAR, TOK_KEYWORD, TOK_RESERVEDOP, TOK_SPECIAL */
    Pos pos;
    Pos end;
} Token;

/* The display form of a token kind, as `aihc-boot lex` prints it. */
char *token_str(const Token *t);

/* An error from any syntax stage, with a position. */
typedef struct {
    const char *stage; /* "lex", "layout" or "parse" */
    Pos pos;
    char *message;
} SyntaxError;

/* A token array. */
typedef struct {
    Token *items;
    size_t len;
    size_t cap;
} TokenVec;

void tokvec_push(TokenVec *v, Token t);

/* --- Lexer and layout --------------------------------------------------- */

typedef struct {
    int magic_hash; /* permit `#` at the end of names, as in `Int#` */
} LexOptions;

/* The options for a source file, from its LANGUAGE pragmas. */
LexOptions lex_options_from_source(const char *src);
/* The extension names in every `{-# LANGUAGE ... #-}` pragma of a file
 * (char *). The scan is textual, so it also works on files the lexer
 * rejects later. */
Vec language_pragmas(const char *src);

/* Each stage returns 0 on success and fills *err on failure. */
int lex(const char *src, size_t len, LexOptions opts, TokenVec *out, SyntaxError *err);
int layout(const TokenVec *in, TokenVec *out, SyntaxError *err);

/* Lex a source file and apply the layout rule. */
int tokenize(const char *src, size_t len, TokenVec *out, SyntaxError *err);

/* --- The syntax tree ---------------------------------------------------- */

/* A possibly qualified name. Operators keep their symbol spelling,
 * without parentheses. The span covers the name as written: with the
 * parentheses of `(+)`, but without the backquotes of `div`. */
struct TcNote;

typedef struct {
    char *qual;
    char *name;
    Span span;
    struct TcNote *note; /* written by the typechecker (tc.h) */
} QName;

/* An unqualified name that a declaration or a pattern binds. The span
 * covers the name as written, as for QName. */
typedef struct {
    char *name;
    Span span;
} Ident;

QName *qname_new(const char *qual, const char *name, Span span);
Ident *ident_new(const char *name, Span span);

typedef struct Type Type;
typedef struct Pat Pat;
typedef struct Expr Expr;
typedef struct Decl Decl;

/* A literal in a type, a pattern or an expression. Every kind keeps the
 * source spelling in raw, so the printer reproduces the literal. */
typedef enum { LIT_INTEGER, LIT_FLOAT, LIT_CHAR, LIT_STRING } LitKind;

typedef struct {
    LitKind kind;
    char *raw;
    unsigned long ch; /* LIT_CHAR */
    char *value;      /* LIT_STRING */
    size_t value_len; /* LIT_STRING */
} Literal;

/* The constructors, fields or methods that come with a type or class. */
typedef enum {
    SUBS_NONE,   /* T */
    SUBS_ALL,    /* T(..) */
    SUBS_SOME,   /* T(A, b) */
    SUBS_ALL_AND /* T(.., P): everything, plus bundled pattern synonyms */
} SubsKind;

typedef struct {
    SubsKind kind;
    Vec names; /* char *, for SUBS_SOME and SUBS_ALL_AND */
} Subs;

typedef enum {
    EXPORT_VAR,     /* name: `map` or `(+)` */
    EXPORT_THING,   /* name, subs, explicit_type */
    EXPORT_PATTERN, /* name: `pattern P` */
    EXPORT_MODULE   /* module: `module M` */
} ExportKind;

typedef struct {
    ExportKind kind;
    QName name;
    Subs subs;
    int explicit_type; /* the `type` namespace keyword */
    char *module;
} Export;

typedef enum {
    IMPORT_VAR,     /* name */
    IMPORT_THING,   /* name, subs, explicit_type */
    IMPORT_PATTERN  /* name */
} ImportItemKind;

typedef struct {
    ImportItemKind kind;
    char *name;
    Subs subs;
    int explicit_type;
} ImportItem;

/* Where the `qualified` keyword of an import stands. */
typedef enum {
    QUAL_NO,
    QUAL_PRE, /* import qualified M */
    QUAL_POST /* import M qualified */
} Qualified;

typedef struct {
    Pos pos;
    char *module;
    Qualified qualified;
    char *package; /* optional: the package qualifier, such as "base" */
    char *alias;   /* optional: `as M` */
    int hiding;
    int has_items; /* 0: everything is imported */
    Vec items;     /* ImportItem * */
} Import;

typedef struct {
    Pos pos;
    Vec pragmas;     /* char *: the pragmas before the header, trimmed */
    char *name;      /* "Main" when the module has no header */
    int has_exports; /* 0: everything is exported */
    Vec exports;     /* Export * */
    Vec imports;     /* Import * */
    Vec decls;       /* Decl * */
} Module;

/* --- Types --- */

/* A type variable binder: `a` or `(a :: k)`. */
typedef struct {
    Ident name;
    Type *kind; /* optional */
} TyVarBind;

typedef enum {
    TY_VAR,      /* var */
    TY_CON,      /* con: `Maybe`, `M.T`, `()`, `[]`, `->` or `(,)` as spelled */
    TY_APP,      /* a b: `f a` */
    TY_APP_KIND, /* a b: `f @k` */
    TY_FUN,      /* a b: `a -> b` */
    TY_OP,       /* a con b: `a `Op` b`, `a :+: b` or `a ~ b` */
    TY_LIST,     /* a span: `[a]`, with the span of the brackets */
    TY_TUPLE,    /* items: `(a, b)`; `()` is TY_CON */
    TY_PAREN,    /* a span: `(t)`, with the span of the parentheses */
    TY_FORALL,   /* binders a: `forall a b. t` */
    TY_QUAL,     /* a b: `ctx => t`; the context is a type */
    TY_KIND_SIG, /* a b: `(t :: k)` without the parentheses */
    TY_PROMOTED, /* a: `'T` */
    TY_PROMOTED_LIST, /* items: `'[a, b]` or `[a, b]` */
    TY_LIT,      /* lit */
    TY_WILDCARD  /* `_` */
} TypeKind;

struct Type {
    TypeKind kind;
    Ident var;
    QName con;
    Type *a;
    Type *b;
    Span span;
    Vec items;   /* Type * */
    Vec binders; /* TyVarBind * */
    Literal lit;
};

/* A constructor argument or record field type, with its strictness. */
typedef struct {
    int strict; /* 0: none, 1: `!`, 2: `~` */
    Type *ty;
} BangType;

/* --- Declarations --- */

typedef enum { ASSOC_LEFT, ASSOC_RIGHT, ASSOC_NONE } Assoc;

/* One record field group: `f, g :: t`. */
typedef struct {
    Vec names; /* Ident * */
    BangType ty;
} Field;

typedef enum {
    CON_PREFIX, /* name args: `C a b` */
    CON_INFIX,  /* left op right: `a :+ b` */
    CON_RECORD  /* name fields: `C { f :: a }` */
} ConBodyKind;

/* One constructor of a data declaration. */
typedef struct {
    Pos pos;
    Vec forall;   /* TyVarBind *: `forall a.` before the constructor */
    Type *ctx;    /* optional: `ctx =>` before the constructor */
    ConBodyKind kind;
    Ident name;   /* CON_PREFIX, CON_RECORD; the operator for CON_INFIX */
    Vec args;     /* BangType *: CON_PREFIX */
    BangType left;  /* CON_INFIX */
    BangType right; /* CON_INFIX */
    Vec fields;   /* Field *: CON_RECORD */
} ConDecl;

typedef enum {
    STRATEGY_NONE,
    STRATEGY_STOCK,
    STRATEGY_ANYCLASS,
    STRATEGY_NEWTYPE
} DerivStrategy;

/* A `deriving` clause. */
typedef struct {
    DerivStrategy strategy;
    Vec classes; /* Type * */
    int parens;  /* the source had parentheses */
    Type *via;   /* optional: `via t` */
} Deriving;

typedef struct Rhs Rhs;

/* The left-hand side of a binding. */
typedef enum {
    LHS_FUN,   /* name args: `f p1 p2` */
    LHS_INFIX, /* left name right args: `p1 op p2`, `(p1 op p2) p3` */
    LHS_PAT    /* pat: `(a, b)` or `!x` */
} LhsKind;

typedef struct {
    LhsKind kind;
    Ident name;
    Vec args;  /* Pat * */
    Pat *left;
    Pat *right;
    Pat *pat;
} Lhs;

typedef enum {
    PATSYN_PREFIX, /* name args: `P a b` */
    PATSYN_INFIX,  /* left op right: `a :< b` */
    PATSYN_RECORD  /* name args: `P {a, b}` */
} PatSynLhsKind;

typedef struct {
    PatSynLhsKind kind;
    char *name;  /* the operator for PATSYN_INFIX */
    Vec args;    /* char *: arguments or fields */
    char *left;
    char *right;
} PatSynLhs;

typedef enum {
    PATSYN_UNIDIRECTIONAL, /* `pattern P <- p` */
    PATSYN_IMPLICIT,       /* `pattern P = p` */
    PATSYN_EXPLICIT        /* `pattern P <- p where P = e` */
} PatSynDirKind;

typedef enum {
    DECL_TYPE_SIG,      /* names ty: `f, g :: t` */
    DECL_DEFAULT_SIG,   /* name ty: `default f :: t` in a class */
    DECL_PAT_SYN_SIG,   /* strs ty: `pattern P :: t` */
    DECL_FIXITY,        /* assoc prec strs: `infixl 5 +, -` */
    DECL_DATA,          /* newtype name vars kind cons deriving */
    DECL_TYPE_SYN,      /* name vars ty: `type T a = t` */
    DECL_TYPE_FAMILY,   /* name vars kind */
    DECL_TYPE_INSTANCE, /* lhs_ty ty: `type instance F Int = t` */
    DECL_CLASS,         /* ctx name vars body */
    DECL_INSTANCE,      /* ty body: the head is a type with any forall
                           and context */
    DECL_BIND,          /* lhs rhs: `f x = e`, `(a, b) = e` */
    DECL_PAT_SYN,       /* patsyn dir body (for PATSYN_EXPLICIT) pat */
    DECL_FOREIGN        /* callconv safety entity name ty:
                           `foreign import prim "e" f :: t` */
} DeclKind;

struct Decl {
    DeclKind kind;
    Pos pos;
    Vec names;      /* Ident *: DECL_TYPE_SIG */
    Ident name;     /* DECL_DEFAULT_SIG, DECL_DATA, DECL_TYPE_SYN,
                       DECL_TYPE_FAMILY, DECL_CLASS */
    Vec strs;       /* char *: DECL_PAT_SYN_SIG names, DECL_FIXITY ops */
    Type *ty;       /* signature type, synonym or instance right-hand
                       side, instance head */
    Assoc assoc;
    char *prec;     /* optional */
    int newtype;
    Vec vars;       /* TyVarBind * */
    Type *kind_sig; /* optional: `data T :: k`, a family's kind */
    Vec cons;       /* ConDecl * */
    Vec deriving;   /* Deriving * */
    Type *lhs_ty;   /* DECL_TYPE_INSTANCE */
    Type *ctx;      /* optional: DECL_CLASS */
    Vec body;       /* Decl *: DECL_CLASS, DECL_INSTANCE, and the where
                       block of PATSYN_EXPLICIT */
    Lhs lhs;        /* DECL_BIND */
    Rhs *rhs;       /* DECL_BIND */
    PatSynLhs patsyn;
    PatSynDirKind dir;
    Pat *pat;       /* DECL_PAT_SYN */
    char *callconv; /* DECL_FOREIGN: `ccall`, `capi` or `prim` */
    char *safety;   /* optional: `safe`, `unsafe` or `interruptible` */
    Literal *entity; /* optional: the string before the name */
    struct TcNote *note; /* written by the typechecker (tc.h) */
};

/* --- Bindings --- */

typedef struct {
    Vec guards; /* Stmt * */
    Expr *body;
} GuardedRhs;

/* The right-hand side of a binding or a case alternative. */
struct Rhs {
    int guarded;  /* 0: `= e` (or `-> e`); 1: `| g = e | g = e` */
    Expr *plain;  /* guarded == 0 */
    Vec guards;   /* GuardedRhs *: guarded == 1 */
    Vec wheres;   /* Decl * */
};

/* --- Patterns --- */

/* A field in a record pattern: `f = p`, or a pun `f`. */
typedef struct {
    QName name;
    Pat *pat; /* optional */
} FieldPat;

/* One `op operand` step of an operator chain. */
typedef struct {
    QName op;
    void *operand; /* Pat * or Expr * */
} InfixStep;

typedef enum {
    PAT_VAR,      /* var */
    PAT_WILDCARD,
    PAT_LIT,      /* lit negative */
    PAT_CON,      /* con args: `C p1 p2`; a bare constructor has no args */
    PAT_INFIX,    /* a steps: `p1 op p2 op p3` in source order */
    PAT_RECORD,   /* con fields wildcard: `C { f = p, g, .. }` */
    PAT_TUPLE,    /* args */
    PAT_LIST,     /* args */
    PAT_PAREN,    /* a */
    PAT_AS,       /* var a: `x@p` */
    PAT_LAZY,     /* a: `~p` */
    PAT_BANG,     /* a: `!p` */
    PAT_VIEW,     /* expr a: `(e -> p)` without the parentheses */
    PAT_SIG,      /* a ty: `p :: t` without the parentheses */
    PAT_TYPE_ARG  /* ty: `@t` in a constructor pattern */
} PatKind;

struct Pat {
    PatKind kind;
    Ident var;
    Literal lit;
    int negative;
    QName con;
    Vec args;    /* Pat * */
    Pat *a;
    Vec steps;   /* InfixStep * with Pat * operands */
    Vec fields;  /* FieldPat * */
    int wildcard;
    Expr *expr;
    Type *ty;
    Span span; /* PAT_LIT only: the literal, without a minus sign */
    struct TcNote *note; /* written by the typechecker (tc.h) */
};

/* --- Expressions --- */

/* A field in a record construction or update: `f = e`, or a pun `f`. */
typedef struct {
    QName name;
    Expr *value; /* optional */
} FieldUpdate;

/* A case alternative. */
typedef struct {
    Pos pos;
    Pat *pat;
    Rhs *rhs;
} Alt;

typedef enum {
    STMT_BIND, /* pat expr: `p <- e` */
    STMT_LET,  /* decls: `let decls` */
    STMT_EXPR  /* expr */
} StmtKind;

/* A statement in a `do` block, a list comprehension or a guard. */
typedef struct {
    StmtKind kind;
    Pat *pat;
    Expr *expr;
    Vec decls; /* Decl * */
    Span span;
} Stmt;

typedef enum {
    EXPR_VAR,         /* name */
    EXPR_CON,         /* name */
    EXPR_LIT,         /* lit */
    EXPR_APP,         /* a b: `f x` */
    EXPR_TYPE_APP,    /* a ty: `f @t` */
    EXPR_INFIX,       /* a steps: `e1 op e2 op e3` in source order */
    EXPR_NEG,         /* a: `-e` */
    EXPR_LAMBDA,      /* pats a: `\p1 p2 -> e` */
    EXPR_LAMBDA_CASE, /* alts: `\case { alts }` */
    EXPR_LET,         /* decls a: `let decls in e` */
    EXPR_IF,          /* a b c */
    EXPR_MULTI_IF,    /* guards: `if | g1 -> e1 | g2 -> e2` (MultiWayIf) */
    EXPR_CASE,        /* a alts */
    EXPR_DO,          /* stmts */
    EXPR_TUPLE,       /* items */
    EXPR_TUPLE_SECTION, /* items: a missing component is NULL */
    EXPR_LIST,        /* items */
    EXPR_ARITH_SEQ,   /* a b c: `[from, then .. to]`; b and c optional */
    EXPR_LIST_COMP,   /* a stmts: `[e | stmts]` */
    EXPR_PAREN,       /* a */
    EXPR_LEFT_SECTION,  /* a name: `(e op)` without the parentheses */
    EXPR_RIGHT_SECTION, /* name a: `(op e)` without the parentheses */
    EXPR_RECORD_CON,  /* name fields wildcard: `C { f = e, g }` */
    EXPR_RECORD_UPDATE, /* a fields: `e { f = e }` */
    EXPR_SIG,         /* a ty: `e :: t` */
    EXPR_HOLE         /* `_` */
} ExprKind;

struct Expr {
    ExprKind kind;
    QName name;
    Literal lit;
    Expr *a;
    Expr *b;
    Expr *c;
    Type *ty;
    Vec steps;  /* InfixStep * with Expr * operands */
    Vec pats;   /* Pat * */
    Vec alts;   /* Alt * */
    Vec decls;  /* Decl * */
    Vec stmts;  /* Stmt * */
    Vec items;  /* Expr * */
    Vec fields; /* FieldUpdate * */
    Vec guards; /* GuardedRhs *: EXPR_MULTI_IF */
    int wildcard;
    Span span;  /* EXPR_LIT, EXPR_LIST, EXPR_TUPLE, EXPR_TUPLE_SECTION,
                   EXPR_NEG and EXPR_ARITH_SEQ only: the whole expression;
                   the resolver reports the syntax terms of these at this
                   span */
    struct TcNote *note; /* written by the typechecker (tc.h) */
};

Type *type_new(TypeKind kind);
Pat *pat_new(PatKind kind);
Expr *expr_new(ExprKind kind);
Decl *decl_new(DeclKind kind, Pos pos);

/* --- Parser and printer ------------------------------------------------- */

/* Parse a module from its tokens. The tokens must come from tokenize. The
 * parser may change the token array. Returns NULL and fills *err on
 * failure. */
Module *parse_module(TokenVec *tokens, SyntaxError *err);

/* Parse a source file into a module. */
Module *parse_source(const char *src, size_t len, SyntaxError *err);

/* Print a module as source text with explicit braces and semicolons. */
char *print_module(const Module *m);

/* The tree in the form of Rust's `{:#?}`, as `aihc-boot parse` prints
 * it. */
char *debug_module(const Module *m);

/* Haskell source spellings of a string and a character literal. */
char *string_literal(const char *s, size_t len);
char *char_literal(unsigned long c);

#endif
