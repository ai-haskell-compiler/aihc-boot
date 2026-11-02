/* The evaluator: a lazy abstract machine over Core, with a copying
 * garbage collector. See eval.c for the machine and "Eval strategy" in
 * docs/PLAN.md for the design.
 */
#ifndef AIHC_EVAL_H
#define AIHC_EVAL_H

#include "core.h"
#include "desugar.h"

typedef struct Obj Obj;

/* A primitive: a function of the evaluator that a `foreign import prim`
 * declaration names. The evaluator evaluates the strict arguments to
 * weak head normal form before the call. The function receives the
 * arguments and returns the result, which the machine then evaluates
 * further when it is a thunk. */
typedef Obj *(*PrimFn)(Obj **args);

struct PrimInfo {
    const char *name;
    unsigned long arity;
    unsigned long strict; /* bit i set: argument i is evaluated first */
    PrimFn fn;
};

/* The primitive with this name, or NULL. */
const PrimInfo *prim_lookup(const char *name);

/* Run the program: evaluate `main` as an IO action. Returns NULL when the
 * program finishes, or an error message. A run-time error of the
 * program itself (such as `error`) prints its message and exits. */
char *eval_run(CoreProgram *program);

#endif
