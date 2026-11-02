/* Core: the untyped language that the interpreter runs.
 *
 * The desugarer (desugar.c) turns the syntax tree of every module into
 * Core, and the evaluator (eval.c) runs it. Core has lambda, application,
 * recursive let, case, literals and saturated constructor applications.
 * Primitives are globals that the evaluator supplies. See "Eval strategy"
 * in docs/PLAN.md.
 *
 * Variables are coordinates, not names. The evaluator keeps the values of
 * local variables in frames: a lambda body sees its arguments as one
 * frame, a let body sees its binders as one frame, and a case alternative
 * sees the fields of its constructor as one frame. A variable names a
 * frame by its distance from the innermost frame, and a slot in that
 * frame. The names stay only for printing.
 *
 * Core is allocated with xmalloc and never freed; the evaluator's heap
 * holds only run-time values.
 */
#ifndef AIHC_CORE_H
#define AIHC_CORE_H

#include <stddef.h>

#include "util.h"

typedef struct Core Core;
typedef struct CoreAlt CoreAlt;
typedef struct ConInfo ConInfo;
typedef struct TypeInfo TypeInfo;
typedef struct Global Global;
typedef struct PrimInfo PrimInfo;

/* A data type: its name and its constructors (ConInfo *), in declaration
 * order. */
struct TypeInfo {
    const char *module;
    const char *name;
    Vec cons;
    int newtype; /* the one constructor is erased */
};

/* A data constructor. */
struct ConInfo {
    const char *name; /* as written: `Just`, `:`, `[]`, `()`, `(,)` */
    int tag;          /* its index among the constructors of its type */
    int arity;        /* with the dictionaries of an existential constructor */
    int ndicts;       /* the dictionaries before the fields */
    Vec field_names;  /* const char *: the record fields, or empty */
    TypeInfo *type;
    void *value; /* the evaluator's static object of a nullary constructor */
};

typedef enum {
    CORE_VAR,    /* up, slot, name: a local variable */
    CORE_GLOBAL, /* global */
    CORE_INT,    /* ival */
    CORE_CHAR,   /* cval */
    CORE_STR,    /* sval, slen: a string literal, which the evaluator
                    unpacks lazily into a list of Char */
    CORE_APP,    /* fun, args: an application to one or more arguments */
    CORE_LAM,    /* arity, body: the arguments are one frame */
    CORE_LET,    /* binds, body: recursive; the binders are one frame */
    CORE_CASE,   /* scrut, alts: force scrut, then run the first alternative
                    that matches; an alternative with fields binds them as
                    one frame */
    CORE_CON     /* con, args: a saturated constructor application */
} CoreKind;

struct Core {
    CoreKind kind;
    unsigned long up;
    unsigned long slot;
    const char *name; /* CORE_VAR: for printing; CORE_LET: unused */
    Global *global;
    long ival;
    unsigned long cval;
    const char *sval;
    size_t slen;
    Core *fun;
    Vec args;     /* Core *: CORE_APP, CORE_CON */
    unsigned long arity;
    Core *body;
    Vec binds;    /* Core *: CORE_LET */
    Vec names;    /* const char *: CORE_LET binder names, CORE_LAM
                     argument names; for printing */
    Core *scrut;
    Vec alts;     /* CoreAlt *: CORE_CASE */
    ConInfo *con;
};

typedef enum {
    ALT_CON,     /* con: binds con->arity fields when arity > 0 */
    ALT_INT,     /* ival */
    ALT_CHAR,    /* cval */
    ALT_DEFAULT  /* anything; binds nothing */
} AltKind;

struct CoreAlt {
    AltKind kind;
    ConInfo *con;
    long ival;
    unsigned long cval;
    Vec names; /* const char *: the field names, for printing */
    Core *body;
};

/* A top-level definition of some module, or a primitive. The evaluator
 * gives every global one heap object. */
struct Global {
    const char *module;
    const char *name;
    Core *body;      /* NULL for a primitive, and before the defining
                        module is desugared */
    const PrimInfo *prim; /* a primitive, or NULL */
    void *value;     /* the evaluator's object (eval.c) */
    unsigned long index;
};

Core *core_new(CoreKind kind);
Core *core_var(unsigned long up, unsigned long slot, const char *name);
Core *core_global(Global *g);
Core *core_int(long value);
Core *core_char(unsigned long value);
Core *core_str(const char *s, size_t len);
/* fun applied to args (Core *). With no args, fun itself. */
Core *core_app(Core *fun, const Vec *args);
Core *core_app1(Core *fun, Core *arg);
Core *core_app2(Core *fun, Core *a, Core *b);
Core *core_lam(unsigned long arity, Core *body);
Core *core_let(const Vec *binds, Core *body);
Core *core_case(Core *scrut, const Vec *alts);
Core *core_con(ConInfo *con, const Vec *args);
CoreAlt *alt_new(AltKind kind, Core *body);

/* Print Core as text, for `aihc-boot core FILE.hs` and the golden
 * tests. */
void core_print(StrBuf *out, const Core *e);
/* Print `NAME = BODY` for a global with a body. */
void core_print_global(StrBuf *out, const Global *g);

#endif
