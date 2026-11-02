/* The desugarer: from the syntax tree of every module to Core.
 *
 * The desugarer handles the monomorphic subset of Haskell that the boot
 * `base` and the eval tests use: no classes and no overloaded literals.
 * A construct outside the subset is an error with the position of the
 * declaration that holds it, so `aihc-boot run` shows what to implement
 * next. See "Eval strategy" in docs/PLAN.md.
 */
#ifndef AIHC_DESUGAR_H
#define AIHC_DESUGAR_H

#include "core.h"
#include "resolve.h"
#include "util.h"

/* The Core of a whole program: every global of every module that the
 * main module reaches through its imports, and the constructors that
 * the evaluator needs by name. */
typedef struct {
    Vec globals;       /* Global *, by index */
    Vec types;         /* TypeInfo * */
    Global *main;      /* Main.main */
    ConInfo *con_false;
    ConInfo *con_true;
    ConInfo *con_nil;
    ConInfo *con_cons;
    ConInfo *con_unit;
    ConInfo *con_iores; /* the result of an IO action: `IORes a` */
} CoreProgram;

/* Desugar the modules that Main reaches. packages holds Package * (see
 * resolve.h), with the main module in a package named "main". The
 * program must hold the same packages. Returns NULL and fills *out on
 * success, or an error message. */
char *desugar_program(const Program *p, const Vec *packages, CoreProgram *out);

#endif
