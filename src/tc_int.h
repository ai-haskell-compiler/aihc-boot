/* The internals of the typechecker, shared by tc_type.c (types and
 * unification), tc_solve.c (constraints), tc_expr.c (expressions,
 * patterns and bindings) and tc_decl.c (declarations and modules).
 */
#ifndef AIHC_TC_INT_H
#define AIHC_TC_INT_H

#include <setjmp.h>

#include "fixity.h"
#include "strmap.h"
#include "tc.h"

/* --- Types --------------------------------------------------------------- */

typedef enum {
    T_META,   /* ref level id: a unification variable */
    T_SKOL,   /* level id name: a rigid type variable */
    T_CON,    /* con: a type constructor */
    T_APP,    /* fun arg */
    T_FORALL, /* vars preds body */
    T_BOUND,  /* id name: a variable that a T_FORALL (or a scheme) binds */
    T_FAM     /* con args: a saturated type family application */
} TyKind;

struct Ty {
    TyKind kind;
    Ty *ref;
    int level;
    unsigned long id;
    const char *name;
    TcTyCon *con;
    Ty *fun;
    Ty *arg;
    Vec vars;  /* Ty *: T_BOUND */
    Vec preds; /* TcPred * */
    Ty *body;
    Vec args;  /* Ty *: T_FAM */
    int scoped; /* T_FORALL: an explicit forall, whose variables scope
                   over the binding (ScopedTypeVariables) */
};

typedef enum {
    TC_DATA,   /* a data type or newtype */
    TC_SYN,    /* a type synonym: params rhs */
    TC_FAMILY, /* a type family: params eqns */
    TC_CLASS,  /* the type-level name of a class */
    TC_PRIM    /* a built-in type: `->`, tuples */
} TyConKind;

/* One equation of a type family: forall vars. F lhs = rhs. */
typedef struct {
    Vec vars; /* Ty *: T_BOUND */
    Vec lhs;  /* Ty * */
    Ty *rhs;
} TcFamEqn;

struct TcTyCon {
    TyConKind kind;
    const char *module;
    const char *name;
    int arity;     /* the number of parameters; -1 when unknown */
    Vec params;    /* Ty *: T_BOUND, for TC_SYN and TC_DATA */
    Ty *rhs;       /* TC_SYN */
    Vec cons;      /* TcCon *: TC_DATA */
    Vec eqns;      /* TcFamEqn *: TC_FAMILY */
    TcClass *cls;  /* TC_CLASS */
    int newtype;
};

struct TcCon {
    const char *module;
    const char *name;
    TcTyCon *tycon;
    int tag;
    Vec univ;        /* Ty *: T_BOUND, the parameters of the type */
    Vec exist;       /* Ty *: T_BOUND, existential variables */
    Vec ctx;         /* TcPred *: the context of an existential constructor */
    Vec fields;      /* Ty * */
    Vec field_names; /* const char *, or NULL for a field without a name */
    Ty *scheme;      /* forall univ exist. ctx => fields -> T univ */
};

/* A class predicate (cls != NULL) or an equality a ~ b. */
struct TcPred {
    TcClass *cls;
    Vec args; /* Ty * */
    Ty *a;
    Ty *b;
};

/* --- Constraints --------------------------------------------------------- */

typedef struct Givens Givens;
struct Givens {
    Givens *up;
    Vec preds; /* TcPred * */
    Vec evs;   /* TcEv * */
};

typedef struct {
    TcPred *pred;
    TcEv *ev;      /* the hole; NULL for an equality */
    Givens *givens;
    Pos pos;
    const char *path;
    int level;
} Wanted;

/* --- Environments -------------------------------------------------------- */

/* A binding group under inference. The occurrences of its members get
 * the group's dictionary parameters after generalization. */
typedef struct {
    Vec occs; /* TcNote * */
} MonoGroup;

typedef struct {
    const char *name;
    Ty *type;
    MonoGroup *mono;
} TcLocal;

typedef struct {
    const char *name;
    Ty *type;
} TcTyVar;

/* A top-level value: its type, and its group while it is inferred. */
typedef struct {
    Ty *type;
    MonoGroup *mono;
} TcVal;

typedef struct {
    char *message;
    Pos pos;
} TcError;

struct Tc {
    const Program *program;
    StrMap tycons;   /* "M\tN" -> TcTyCon * */
    StrMap cons;     /* "M\tN" -> TcCon * */
    StrMap classes;  /* "M\tN" -> TcClass * */
    StrMap values;   /* "M\tN" -> TcVal * */
    StrMap fixities; /* "M\tN" -> Fixity * */
    StrMap infos;    /* module -> TcModuleInfo * */
    StrMap errors;   /* module -> TcError *, NULL value: ok */
    StrMap tuples;   /* "(,)" -> TcTyCon * */
    TcTyCon *tc_fun;
    TcTyCon *tc_unit;
    /* The current module. */
    const char *module;
    const char *path;
    const Scope *scope;
    TcModuleInfo *info;
    int overloaded_strings;
    int scoped_tyvars;
    /* The current position, for errors. */
    Pos pos;
    int level;
    Vec wanted;    /* Wanted * */
    Givens *givens;
    TcLocal *locals;
    size_t nlocals;
    size_t cap_locals;
    Vec tyvars;    /* TcTyVar * */
    unsigned long next_id;
    unsigned long next_dict;
    jmp_buf jump;
    char *error;
};

/* --- tc_type.c ----------------------------------------------------------- */

void tc_fail(Tc *tc, char *message);

char *tkey(const char *module, const char *name);

Ty *ty_new(TyKind kind);
Ty *ty_meta(Tc *tc);
Ty *ty_skol(Tc *tc, const char *name);
Ty *ty_bound(Tc *tc, const char *name);
Ty *ty_con(TcTyCon *con);
Ty *ty_app(Ty *f, Ty *a);
Ty *ty_fun(Tc *tc, Ty *a, Ty *b);
Ty *ty_apps(Ty *f, const Vec *args);
Ty *ty_forall(const Vec *vars, const Vec *preds, Ty *body);
Ty *prune(Ty *t);
/* Reduce type family applications at the head, as far as the equations
 * allow. */
Ty *ty_whnf(Tc *tc, Ty *t);
/* Whether t is a function type; sets *a and *b. */
int ty_is_fun(Tc *tc, Ty *t, Ty **a, Ty **b);
/* The head and the arguments of a type application. */
Ty *ty_spine(Ty *t, Vec *args);
/* Substitute T_BOUND ids (Ty * in from) by the types in to. */
Ty *ty_subst(Ty *t, const Vec *from, const Vec *to);
TcPred *pred_subst(const TcPred *p, const Vec *from, const Vec *to);
TcPred *pred_new(TcClass *cls, const Vec *args);
/* A fully pruned copy. */
Ty *zonk(Tc *tc, Ty *t);
TcPred *pred_zonk(Tc *tc, const TcPred *p);
void unify(Tc *tc, Ty *a, Ty *b);
/* Unify without failing: returns 0 on success. Changes made before a
 * failure stay. */
int try_unify(Tc *tc, Ty *a, Ty *b);
/* Structural equality after pruning. */
int ty_equal(Tc *tc, Ty *a, Ty *b);
int pred_equal(Tc *tc, const TcPred *a, const TcPred *b);
/* One-way match of a pattern (with T_BOUND variables in vars) against a
 * type. Fills to (Ty * for each var, NULL when unbound). */
int ty_match(Tc *tc, Ty *pat, Ty *t, const Vec *vars, Vec *to);
void ty_print(StrBuf *b, Ty *t);
char *ty_str(Ty *t);
/* The type in parentheses unless it is atomic. */
char *ty_str_atom(Ty *t);
char *pred_str(const TcPred *p);
/* The meta variables of a type (Ty *), without duplicates. */
void ty_metas(Tc *tc, Ty *t, Vec *out);
void pred_metas(Tc *tc, const TcPred *p, Vec *out);
int ty_has_skolem(Tc *tc, Ty *t, const Vec *skolems);

/* Instantiate the outer foralls of a type with fresh metas. The
 * predicates become wanteds; their holes go to evs (TcEv *) when evs is
 * not NULL. */
Ty *instantiate(Tc *tc, Ty *t, Vec *evs);
/* Instantiate one forall with the given types. */
Ty *instantiate_with(Tc *tc, Ty *t, const Vec *types, Vec *evs);

/* --- tc_solve.c ---------------------------------------------------------- */

TcEv *ev_new(TcEvKind kind);
TcEv *emit_wanted(Tc *tc, TcPred *p);
void emit_equality(Tc *tc, Ty *a, Ty *b);
char *fresh_dict(Tc *tc);
/* Push a scope of givens; each predicate gets a fresh dictionary
 * variable, whose name goes to dicts. */
void push_givens(Tc *tc, const Vec *preds, Vec *dicts);
void push_given_evs(Tc *tc, const Vec *preds, const Vec *evs);
void pop_givens(Tc *tc);
/* Solve as many wanteds as possible. */
void solve(Tc *tc);
/* Default the ambiguous variables of the wanteds, then solve. Fails on
 * any wanted that stays. */
void solve_final(Tc *tc);
/* Try to default the wanteds whose metas are all in vars (Ty *). */
int apply_defaults(Tc *tc, Vec *wanteds);
/* Report the first unsolved wanted. */
void fail_wanted(Tc *tc, const Wanted *w);

/* --- tc_expr.c ----------------------------------------------------------- */

void local_push(Tc *tc, const char *name, Ty *type, MonoGroup *mono);
const TcLocal *local_lookup(const Tc *tc, const char *name);
void tyvar_push(Tc *tc, const char *name, Ty *type);
Ty *tyvar_lookup(const Tc *tc, const char *name);

void tc_expr(Tc *tc, const Expr *e, Ty *expected);
/* Check against a type that may be a forall. */
void tc_expr_sigma(Tc *tc, const Expr *e, Ty *sigma);
/* Check a group of declarations (top-level or local) and bind their
 * names. top: the names are top-level values of the current module. */
void tc_bind_group(Tc *tc, const Vec *decls, int top);
/* Check the clauses of one binding against a type with a known
 * signature. The note of the first clause gets the dictionaries. */
void tc_check_binding(Tc *tc, const Vec *clauses, Ty *sigma, const char *name);
/* The fixity of an operator. */
Fixity tc_fixity(Tc *tc, const QName *op);

/* --- tc_decl.c ----------------------------------------------------------- */

/* The entity that a name denotes in the current scope, or NULL. */
const Entity *tc_entity(Tc *tc, const QName *name, Space space);
TcVal *tc_value(Tc *tc, const char *module, const char *name);
TcCon *tc_con(Tc *tc, const QName *name);
TcTyCon *tc_tuple(Tc *tc, size_t n);
TcTyCon *tc_known_tycon(Tc *tc, const char *module, const char *name);
TcClass *tc_known_class(Tc *tc, const char *module, const char *name);
Ty *tc_known_type(Tc *tc, const char *module, const char *name);
Ty *ty_list(Tc *tc, Ty *elem);
/* Convert a syntax type: a signature with implicit quantification. */
Ty *conv_sigma(Tc *tc, const Type *t);
/* Convert a type in a pattern or expression signature: free variables
 * that are not in scope become fresh metas, and stay in scope. */
Ty *conv_open(Tc *tc, const Type *t);

#endif
