/* Name resolution.
 *
 * The resolver gives every identifier occurrence of a module a target: the
 * top-level entity it names, a local binder, or built-in syntax. The
 * reference is aihc-resolve, run by `tools/resolve-oracle`. Both sides
 * print the same records (see record.c), and resolve_compare decides
 * whether a module resolves the same way in both.
 *
 * A Program holds the top-level names of every package (see scope.c), and
 * resolve_module resolves one module of it (see resolve.c). The resolver
 * handles the constructs of the vendored `base` and `deepseq`. It refuses
 * the others with a "not supported yet" error.
 */
#ifndef AIHC_RESOLVE_H
#define AIHC_RESOLVE_H

#include "strmap.h"
#include "syntax.h"
#include "util.h"

/* --- Records (record.c) ------------------------------------------------- */

/* The source range of one identifier. Lines and columns start at 1. The
 * end is exclusive. Spans compare by start line, start column, end line
 * and end column, in this order. */
typedef struct {
    unsigned long start_line;
    unsigned long start_col;
    unsigned long end_line;
    unsigned long end_col;
} RecSpan;

/* -1, 0 or 1. */
int recspan_cmp(RecSpan a, RecSpan b);
/* The span as `L1:C1-L2:C2`. */
char *recspan_str(RecSpan s);

typedef enum { NS_TERM, NS_TYPE, NS_MODULE } Namespace;

const char *namespace_str(Namespace ns);

/* What an identifier occurrence names. */
typedef enum {
    TARGET_TOP,    /* package module name: a top-level entity, by the
                      package and module that define it */
    TARGET_LOCAL,  /* id: a local binder; the id is unique only in its file */
    TARGET_SYNTAX, /* built-in syntax, such as a tuple constructor */
    TARGET_ERROR   /* message: the occurrence does not resolve */
} TargetKind;

typedef struct {
    TargetKind kind;
    const char *package;
    const char *module;
    const char *name;
    unsigned long id;
    const char *message;
} Target;

/* Whether two targets are equal: the same kind and the same fields. */
int target_eq(const Target *a, const Target *b);
/* The target in record form: `top\tP\tM\tN`, `local\tID`, `syntax` or
 * `error\tMESSAGE`. */
void target_write(StrBuf *b, const Target *t);

typedef struct {
    RecSpan span;
    Namespace ns;
    Target target;
} Occurrence;

/* An array of occurrences. */
typedef struct {
    Occurrence *items;
    size_t len;
    size_t cap;
} OccVec;

void occvec_init(OccVec *v);
void occvec_push(OccVec *v, Occurrence o);
/* Sort by span. The sort is stable, as Rust's sort_by_key. */
void occvec_sort(OccVec *v);

/* One module of a dump: whether it resolved, and its occurrences sorted
 * by span. */
typedef struct {
    char *error; /* NULL when the module resolved */
    OccVec occs;
} ModuleRecords;

/* A whole dump, by file. The files are sorted in byte order. The values
 * are ModuleRecords *. */
typedef struct {
    StrMap modules;
} Dump;

void dump_init(Dump *d);
/* The records of file. Adds empty records when the dump does not have
 * the file. */
ModuleRecords *dump_entry(Dump *d, const char *file);
/* Read a dump from its text form. Returns NULL on success, or the
 * message for a line that is not a record, with its line number. */
char *dump_parse(const char *text, Dump *out);
/* Print a dump in its text form. */
void dump_write(const Dump *d, StrBuf *out);

/* --- Errors ------------------------------------------------------------- */

/* The resolver could not resolve a module. */
typedef struct {
    char *message;
    int has_pos;
    Pos pos;
} ResolveError;

ResolveError *resolve_error(char *message, const Pos *pos);
/* A resolve error for a construct that the resolver does not handle yet:
 * "resolve: WHAT is not supported yet". pos is optional. */
ResolveError *unsupported(const char *what, const Pos *pos);

/* --- Scopes and programs (scope.c) -------------------------------------- */

/* A top-level entity: the package and module that define it, and its
 * name. */
typedef struct {
    const char *package;
    const char *module;
    const char *name;
} Entity;

Target entity_target(const Entity *e);

/* Names in scope: terms and types, unqualified and by qualifier. The
 * values of terms and types are Entity *; the values of qualified are
 * Scope *. */
typedef struct Scope Scope;
struct Scope {
    StrMap terms;
    StrMap types;
    StrMap qualified;
};

Scope *scope_new(void);
/* Add the names of other that this scope does not have yet. The names
 * already in the scope win. */
void scope_union(Scope *s, const Scope *other);
/* The names under a qualifier, or the scope itself for an empty
 * qualifier. NULL when the scope does not have the qualifier. */
const Scope *scope_qualifier(const Scope *s, const char *qualifier);

/* Whether a name is in the term or the type namespace. */
typedef enum { SPACE_TERM, SPACE_TYPE } Space;

/* A child of a type or class: a constructor, a field, a method or an
 * associated type. */
typedef struct {
    Space space;
    const char *name;
} Child;

/* One module of a package, with the file it comes from. */
typedef struct {
    char *path;
    Module *module;
} SourceModule;

/* One package: its name, the extensions of its `.cabal` file (char *,
 * such as `NoImplicitPrelude`), and its modules (SourceModule *). */
typedef struct {
    char *name;
    Vec extensions;
    Vec modules;
} Package;

/* The builtin modules for a set of packages (char *): the modules whose
 * exports the syntax uses without an import. `tools/resolve-oracle` gets
 * the same list from the manifest. Returns a Vec of char *. */
Vec builtin_modules(const Vec *packages);

typedef struct Program Program;

/* A program whose syntax takes `:` and `~` from builtin_modules (char *). */
Program *program_new(const Vec *builtin_modules);
/* Add a package. Its dependencies must be in the program already. */
void program_add_package(Program *p, const Package *package);
/* The module that a file holds, or NULL. Sets *package when package is
 * not NULL. */
const Module *program_module_at(const Program *p, const char *path,
                                const char **package);
/* The top-level scope of a module. Returns NULL and sets *err when the
 * module does not resolve. */
const Scope *program_scope(const Program *p, const char *module,
                           ResolveError **err);
/* The constructors, fields, methods and associated types of a type or
 * class (Child *). NULL when there are none. */
const Vec *program_children(const Program *p, const Entity *parent);

/* A name without the backquotes of `op` in backquotes. */
const char *bare(const char *name);
/* Whether a module gets the implicit `import Prelude`. */
int implicit_prelude(const Vec *extensions, const Module *module);

/* --- Resolution (resolve.c) --------------------------------------------- */

/* Every identifier occurrence of the module in the file at path, with its
 * target, sorted by span. The module's package must be in the program.
 * Returns NULL on success. Returns an error when the module uses a
 * construct that the resolver does not handle yet, or when it imports a
 * module that does not resolve. */
ResolveError *resolve_module(const Program *p, const char *path, OccVec *out);

/* --- Comparison (compare.c) --------------------------------------------- */

/* Where two resolutions of one module differ. */
typedef struct {
    RecSpan span;
    char *message;
} Disagreement;

/* Whether our occurrences agree with the reference's occurrences of the
 * same module. Both lists must be sorted by span. Returns 0 when they
 * agree. Returns 1 and fills *d with the first disagreement, in span
 * order, when they do not.
 *
 * The rules:
 *
 * - Both sides see the same spans.
 * - The namespaces agree.
 * - A top-level target agrees when package, module and name are equal.
 * - Local targets agree when they induce the same partition of the
 *   spans: two spans share a local id on one side exactly when they do
 *   on the other. The comparison does not compare the ids themselves.
 * - Syntax agrees with syntax, and an error with an error. The
 *   comparison does not compare error messages.
 */
int resolve_compare(const OccVec *ours, const OccVec *theirs, Disagreement *d);

#endif
