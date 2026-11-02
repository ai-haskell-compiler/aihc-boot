/* The typechecker.
 *
 * The typechecker infers and checks the types of every module, in an
 * order where each module comes after the modules it imports. It is a
 * reduced form of OutsideIn(X), as docs/PLAN.md describes: type
 * inference makes wanted constraints, and the solver solves them with
 * the given constraints of signatures and instance contexts, and with
 * the instances.
 *
 * The output is the evidence for the dictionary-passing translation.
 * The typechecker writes notes (TcNote) into the syntax tree: the
 * dictionaries that an occurrence of an overloaded name takes, and the
 * dictionary parameters that a binding takes. The desugarer reads the
 * notes and the classes and instances of each module (TcModuleInfo),
 * and makes Core with explicit dictionaries.
 *
 * A name with a qualifier that starts with TC_DIRECT names a top-level
 * entity directly: the rest of the qualifier is the module. Derived
 * instances and default methods use these names, because their code
 * does not come from the module's scope.
 */
#ifndef AIHC_TC_H
#define AIHC_TC_H

#include "resolve.h"
#include "syntax.h"
#include "util.h"

#define TC_DIRECT '\001'

typedef struct Ty Ty;
typedef struct TcPred TcPred;
typedef struct TcEv TcEv;
typedef struct TcClass TcClass;
typedef struct TcInst TcInst;
typedef struct TcNote TcNote;
typedef struct TcTyCon TcTyCon;
typedef struct TcCon TcCon;

/* --- Evidence ------------------------------------------------------------ */

typedef enum {
    EV_HOLE,    /* ref: the solution, or NULL while the solver works */
    EV_VAR,     /* var: a dictionary variable */
    EV_INST,    /* inst args: an instance applied to the dictionaries of its
                   context */
    EV_SUPER,   /* of cls index: a superclass of the dictionary `of` */
    EV_TYPEREP  /* cls rep: the Typeable dictionary of a type */
} TcEvKind;

struct TcEv {
    TcEvKind kind;
    TcEv *ref;
    const char *var;
    TcInst *inst;
    Vec args; /* TcEv * */
    TcEv *of;
    TcClass *cls;
    int index;
    const char *rep;
};

/* The evidence with the solved holes followed. */
TcEv *tc_ev(TcEv *e);

/* --- Notes --------------------------------------------------------------- */

/* A note on a piece of syntax (the `note` field of QName, Expr, Pat and
 * Decl). What the fields mean depends on the syntax:
 *
 * - QName of a variable or constructor occurrence: evs are the
 *   dictionaries that the occurrence takes, in order.
 * - Expr: EXPR_LIT: evs[0] is the Num (or IsString) dictionary.
 *   EXPR_NEG: evs[0] is the Num
 *   dictionary. EXPR_DO: evs[0] is the Monad dictionary. EXPR_ARITH_SEQ:
 *   evs[0] is the Enum dictionary. EXPR_APP: evs are dictionaries that
 *   the result of the application takes (a rank-n field).
 * - Pat: PAT_LIT: evs are the Eq and the Num (or IsString) dictionary.
 *   PAT_CON: dicts are the dictionary
 *   variables that an existential constructor binds before its fields.
 * - Decl (the first clause of a binding): dicts are the dictionary
 *   parameters of the binding.
 * - Any Expr: lams, when not empty, are dictionary parameters around the
 *   expression. EXPR_SIG: evs are applied after them.
 */
struct TcNote {
    Vec evs;   /* TcEv * */
    Vec dicts; /* const char * */
    Vec lams;  /* const char *: Expr only: the expression takes these
                  dictionaries (it was checked against a type with a
                  context) */
};

TcNote *tc_note_new(void);

/* --- Classes and instances ---------------------------------------------- */

/* A class. Its dictionary has the superclasses first, then the
 * methods. */
struct TcClass {
    const char *module;
    const char *name;
    int nparams;
    Vec params;   /* Ty *: T_BOUND */
    Vec supers;   /* TcPred * over params */
    Vec methods;  /* TcMethod * */
    Vec fundeps;  /* TcFunDep * */
    Vec assoc;    /* TcTyCon *: associated type families */
    Vec instances; /* TcInst * */
    int typeable; /* the built-in class Typeable */
};

typedef struct {
    const char *name;
    Ty *sig;        /* the signature, over the class parameters */
    Ty *scheme;     /* forall params. C params => sig */
    Vec *deflt;     /* Decl *: the clauses of the default, or NULL */
    Ty *dm_scheme;  /* the type of the default method: the default
                       signature, or the scheme */
} TcMethod;

/* The name of the global that holds the default of a method. */
char *tc_default_name(const char *method);

typedef struct {
    Vec from; /* int indexes of the class parameters */
    Vec to;
} TcFunDep;

struct TcInst {
    TcClass *cls;
    const char *module;
    const char *name;  /* the global that holds the dictionary function */
    const char *path;  /* the file of the declaration, for errors */
    Vec vars;          /* Ty *: T_BOUND */
    Vec ctx;           /* TcPred * */
    Vec head;          /* Ty *: one for each class parameter */
    const Decl *decl;  /* the instance declaration, or NULL when derived */
    Vec body;          /* Decl *: the bindings (also for derived ones) */
    /* For the desugarer. */
    Vec ctx_dicts;     /* const char *: the dictionary parameters */
    Vec supers;        /* TcEv *: the superclass dictionaries */
    Vec methods;       /* for each method: Vec * of Decl * clauses, or NULL
                          when the method is missing */
    Vec assoc;         /* type family equations: TcFamEqn * */
};

/* The classes, instances and other generated code of one module. */
typedef struct {
    Vec classes;   /* TcClass * */
    Vec instances; /* TcInst * */
    Vec selectors; /* TcSelector *: record field selectors */
} TcModuleInfo;

/* A record field selector: the global `name` takes field `index` of
 * each constructor that has the field. */
typedef struct {
    const char *name;
    const char *type_module;
    Vec cons;    /* const char *: constructor names */
    Vec indexes; /* the field index for each constructor, as (void *)(size_t) */
} TcSelector;

/* --- Running ------------------------------------------------------------- */

typedef struct Tc Tc;

Tc *tc_new(const Program *p);

/* Typecheck the modules of a package, each after the modules it
 * imports. The package's dependencies must be typechecked already. */
void tc_package(Tc *tc, const Package *package);

/* The error of a module, or NULL when it typechecked. Sets *pos (line 0
 * when unknown). A module that was never checked gives an error. */
const char *tc_module_error(const Tc *tc, const char *module, Pos *pos);

const TcModuleInfo *tc_info(const Tc *tc, const char *module);

/* Whether a Num evidence is the instance for Int. */
int tc_ev_is_int(TcEv *e);

#endif
