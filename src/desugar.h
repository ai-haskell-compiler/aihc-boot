/* The desugarer: from the syntax tree of every module to Core.
 *
 * Classes become dictionaries: a class is a data type with one
 * constructor, whose fields are the superclasses and the methods; an
 * instance is a global that builds the dictionary; an overloaded name
 * takes its dictionaries as arguments. The typechecker (tc.h) says
 * which. A construct that the desugarer does not handle is an error
 * with the position of the declaration that holds it, so `aihc-boot
 * run` shows what to implement next. See "Eval strategy" in
 * docs/PLAN.md.
 */
#ifndef AIHC_DESUGAR_H
#define AIHC_DESUGAR_H

#include "core.h"
#include "resolve.h"
#include "tc.h"
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
 * program and the typechecker must hold the same packages: the
 * desugarer takes the dictionaries from the typechecker's notes.
 * Returns NULL and fills *out on success, or an error message. */
char *desugar_program(const Program *p, const Tc *tc, const Vec *packages, CoreProgram *out);

#endif
