/* The top-level names of the vendored packages: what each module
 * defines, imports and exports.
 *
 * The rules follow aihc-resolve, the reference resolver:
 *
 * - A module's own top-level names come first. Then come its imports, in
 *   source order: when two imports give the same name, the first import
 *   wins. Then comes the implicit `Prelude` import. Last come the list
 *   constructor `:` and the equality type `~` from the builtin modules.
 * - A module without an export list exports its own top-level names only.
 */
#include <stdlib.h>
#include <string.h>

#include "resolve.h"

/* --- Errors ------------------------------------------------------------- */

ResolveError *resolve_error(char *message, const Pos *pos)
{
    ResolveError *e = (ResolveError *)xmalloc(sizeof *e);
    e->message = message;
    e->has_pos = pos != NULL;
    if (pos != NULL) {
        e->pos = *pos;
    } else {
        e->pos.line = 0;
        e->pos.col = 0;
    }
    return e;
}

ResolveError *unsupported(const char *what, const Pos *pos)
{
    return resolve_error(str_printf("resolve: %s is not supported yet", what), pos);
}

/* --- Entities and scopes ------------------------------------------------ */

Target entity_target(const Entity *e)
{
    Target t;
    memset(&t, 0, sizeof t);
    t.kind = TARGET_TOP;
    t.package = e->package;
    t.module = e->module;
    t.name = e->name;
    return t;
}

static Entity *entity_new(const char *package, const char *module, const char *name)
{
    Entity *e = (Entity *)xmalloc(sizeof *e);
    e->package = package;
    e->module = module;
    e->name = name;
    return e;
}

Scope *scope_new(void)
{
    Scope *s = (Scope *)xmalloc(sizeof *s);
    strmap_init(&s->terms);
    strmap_init(&s->types);
    strmap_init(&s->qualified);
    return s;
}

/* A deep copy of a scope. The entities are shared; they do not change. */
static Scope *scope_clone(const Scope *s)
{
    Scope *c = scope_new();
    size_t i;
    c->terms = strmap_copy(&s->terms);
    c->types = strmap_copy(&s->types);
    for (i = 0; i < s->qualified.len; i++) {
        strmap_put(&c->qualified, s->qualified.keys[i],
                   scope_clone((const Scope *)s->qualified.vals[i]));
    }
    return c;
}

void scope_union(Scope *s, const Scope *other)
{
    size_t i;
    strmap_union(&s->terms, &other->terms);
    strmap_union(&s->types, &other->types);
    for (i = 0; i < other->qualified.len; i++) {
        Scope *q = (Scope *)strmap_get(&s->qualified, other->qualified.keys[i]);
        if (q == NULL) {
            q = scope_new();
            strmap_put(&s->qualified, other->qualified.keys[i], q);
        }
        scope_union(q, (const Scope *)other->qualified.vals[i]);
    }
}

/* The scope under qualifier in s->qualified. Adds an empty scope when s
 * does not have the qualifier. */
static Scope *scope_qualified_entry(Scope *s, const char *qualifier)
{
    Scope *q = (Scope *)strmap_get(&s->qualified, qualifier);
    if (q == NULL) {
        q = scope_new();
        strmap_put(&s->qualified, qualifier, q);
    }
    return q;
}

/* The unqualified names of a scope, without its qualifiers. */
static Scope *scope_unqualified(const Scope *s)
{
    Scope *c = scope_new();
    c->terms = strmap_copy(&s->terms);
    c->types = strmap_copy(&s->types);
    return c;
}

const Scope *scope_qualifier(const Scope *s, const char *qualifier)
{
    if (qualifier[0] == '\0') {
        return s;
    }
    return (const Scope *)strmap_get(&s->qualified, qualifier);
}

/* --- Packages and programs ---------------------------------------------- */

Vec builtin_modules(const Vec *packages)
{
    Vec out;
    vec_init(&out);
    if (vec_has_str(packages, "base")) {
        vec_push(&out, "GHC.Types");
        vec_push(&out, "GHC.Classes");
        vec_push(&out, "GHC.Num");
        vec_push(&out, "GHC.Base");
        vec_push(&out, "GHC.Enum");
        vec_push(&out, "Data.String");
    }
    return out;
}

/* The resolved top-level names of a module. Each result is a scope, or an
 * error. */
typedef struct {
    /* Everything in scope at the top level of the module. */
    Scope *scope;
    ResolveError *scope_err;
    Scope *exports;
    ResolveError *exports_err;
} ModuleData;

/* The module of a file, with its package. */
typedef struct {
    const char *package;
    Module *module;
} PathEntry;

/* Every package that the resolver knows, and the top-level names of their
 * modules. */
struct Program {
    /* The modules by name (ModuleData *). Module names are unique in the
     * vendored tree. */
    StrMap modules;
    /* The module of each file, by path (PathEntry *). */
    StrMap paths;
    /* The constructors and fields of each type, and the methods and
     * associated types of each class (Vec * of Child *), by entity_key. */
    StrMap children;
    /* The modules whose `:` and `~` every module sees without an import
     * (char *). */
    Vec builtin_modules;
};

/* The key of an entity in Program.children. */
static char *entity_key(const Entity *e)
{
    return str_printf("%s\001%s\001%s", e->package, e->module, e->name);
}

Program *program_new(const Vec *builtin)
{
    Program *p = (Program *)xmalloc(sizeof *p);
    size_t i;
    strmap_init(&p->modules);
    strmap_init(&p->paths);
    strmap_init(&p->children);
    vec_init(&p->builtin_modules);
    for (i = 0; i < builtin->len; i++) {
        vec_push(&p->builtin_modules, xstrdup((const char *)builtin->items[i]));
    }
    return p;
}

const Module *program_module_at(const Program *p, const char *path,
                                const char **package)
{
    const PathEntry *e = (const PathEntry *)strmap_get(&p->paths, path);
    if (e == NULL) {
        return NULL;
    }
    if (package != NULL) {
        *package = e->package;
    }
    return e->module;
}

const Scope *program_scope(const Program *p, const char *module,
                           ResolveError **err)
{
    const ModuleData *data = (const ModuleData *)strmap_get(&p->modules, module);
    char *what;
    if (data != NULL) {
        if (data->scope == NULL) {
            *err = data->scope_err;
        }
        return data->scope;
    }
    what = str_printf("module %s", module);
    *err = unsupported(what, NULL);
    return NULL;
}

Target program_builtin(const Program *p, const char *module, Space space,
                       const char *name)
{
    size_t i;
    Target t;
    (void)module;
    for (i = 0; i < p->builtin_modules.len; i++) {
        const char *builtin = (const char *)p->builtin_modules.items[i];
        const ModuleData *data;
        const Entity *entity;
        data = (const ModuleData *)strmap_get(&p->modules, builtin);
        if (data == NULL || data->exports == NULL) {
            continue;
        }
        entity = (const Entity *)strmap_get(
            space == SPACE_TERM ? &data->exports->terms : &data->exports->types, name);
        if (entity != NULL) {
            return entity_target(entity);
        }
    }
    memset(&t, 0, sizeof t);
    t.kind = TARGET_ERROR;
    t.message = "unbound";
    return t;
}

const Vec *program_children(const Program *p, const Entity *parent)
{
    return (const Vec *)strmap_get(&p->children, entity_key(parent));
}

/* The names that every module sees without an import: `:` and `~` from
 * the builtin modules. A builtin module that is `module` itself, or that
 * does not resolve, gives nothing. */
static Scope *builtin_scope(const Program *p, const char *module)
{
    Scope *builtin = scope_new();
    size_t i;
    for (i = 0; i < p->builtin_modules.len; i++) {
        const char *name = (const char *)p->builtin_modules.items[i];
        const ModuleData *data;
        void *entity;
        if (str_eq(name, module)) {
            continue;
        }
        data = (const ModuleData *)strmap_get(&p->modules, name);
        if (data == NULL || data->exports == NULL) {
            continue;
        }
        entity = strmap_get(&data->exports->terms, ":");
        if (entity != NULL) {
            strmap_put(&builtin->terms, ":", entity);
        }
        entity = strmap_get(&data->exports->types, "~");
        if (entity != NULL) {
            strmap_put(&builtin->types, "~", entity);
        }
    }
    return builtin;
}

const char *bare(const char *name)
{
    size_t n = strlen(name);
    if (n >= 2 && name[0] == '`' && name[n - 1] == '`') {
        return xstrndup(name + 1, n - 2);
    }
    return name;
}

/* The variables that a pattern binds (char *), in source order. Returns 0
 * for a pattern that the resolver does not handle. */
static int pattern_vars(const Pat *pat, Vec *out)
{
    size_t i;
    switch (pat->kind) {
    case PAT_VAR:
        vec_push(out, pat->var.name);
        return 1;
    case PAT_WILDCARD:
        return 1;
    case PAT_CON:
    case PAT_TUPLE:
    case PAT_LIST:
        for (i = 0; i < pat->args.len; i++) {
            if (!pattern_vars((const Pat *)pat->args.items[i], out)) {
                return 0;
            }
        }
        return 1;
    case PAT_INFIX:
        if (!pattern_vars(pat->a, out)) {
            return 0;
        }
        for (i = 0; i < pat->steps.len; i++) {
            const InfixStep *step = (const InfixStep *)pat->steps.items[i];
            if (!pattern_vars((const Pat *)step->operand, out)) {
                return 0;
            }
        }
        return 1;
    case PAT_PAREN:
    case PAT_LAZY:
    case PAT_BANG:
        return pattern_vars(pat->a, out);
    case PAT_AS:
        vec_push(out, pat->var.name);
        return pattern_vars(pat->a, out);
    default:
        return 0;
    }
}

/* The constructor and the field names of one constructor declaration
 * (char *). */
static Vec con_children(const ConDecl *con)
{
    Vec out;
    size_t i;
    size_t j;
    vec_init(&out);
    switch (con->kind) {
    case CON_PREFIX:
        if (!str_eq(con->name.name, "[]")) {
            vec_push(&out, con->name.name);
        }
        break;
    case CON_INFIX:
        vec_push(&out, (char *)bare(con->name.name));
        break;
    case CON_RECORD:
        vec_push(&out, con->name.name);
        for (i = 0; i < con->fields.len; i++) {
            const Field *f = (const Field *)con->fields.items[i];
            for (j = 0; j < f->names.len; j++) {
                vec_push(&out, ((const Ident *)f->names.items[j])->name);
            }
        }
        break;
    case CON_GADT:
        for (i = 0; i < con->names.len; i++) {
            vec_push(&out, (char *)bare(((const Ident *)con->names.items[i])->name));
        }
        break;
    }
    return out;
}

static void add_term(Scope *s, const char *package, const Module *module,
                     const char *name)
{
    strmap_put(&s->terms, name, entity_new(package, module->name, name));
}

static void add_type(Scope *s, const char *package, const Module *module,
                     const char *name)
{
    strmap_put(&s->types, name, entity_new(package, module->name, name));
}

/* The names that a module defines at its top level. Returns NULL and
 * sets *err on failure. */
static Scope *own_scope(const char *package, const Module *module, ResolveError **err)
{
    Scope *scope = scope_new();
    size_t i;
    size_t j;
    size_t k;
    for (i = 0; i < module->decls.len; i++) {
        const Decl *decl = (const Decl *)module->decls.items[i];
        switch (decl->kind) {
        case DECL_BIND:
            switch (decl->lhs.kind) {
            case LHS_FUN:
                add_term(scope, package, module, decl->lhs.name.name);
                break;
            case LHS_INFIX:
                add_term(scope, package, module, bare(decl->lhs.name.name));
                break;
            case LHS_PAT: {
                Vec vars;
                vec_init(&vars);
                if (!pattern_vars(decl->lhs.pat, &vars)) {
                    *err = unsupported("this top-level pattern binding", &decl->pos);
                    return NULL;
                }
                for (j = 0; j < vars.len; j++) {
                    add_term(scope, package, module, (const char *)vars.items[j]);
                }
                break;
            }
            }
            break;
        case DECL_DATA:
            add_type(scope, package, module, decl->name.name);
            for (j = 0; j < decl->cons.len; j++) {
                Vec children = con_children((const ConDecl *)decl->cons.items[j]);
                for (k = 0; k < children.len; k++) {
                    add_term(scope, package, module, (const char *)children.items[k]);
                }
            }
            break;
        case DECL_TYPE_SYN:
        case DECL_TYPE_FAMILY:
            add_type(scope, package, module, decl->name.name);
            break;
        case DECL_CLASS:
            add_type(scope, package, module, decl->name.name);
            for (j = 0; j < decl->body.len; j++) {
                const Decl *item = (const Decl *)decl->body.items[j];
                if (item->kind == DECL_TYPE_SIG) {
                    for (k = 0; k < item->names.len; k++) {
                        add_term(scope, package, module,
                                 ((const Ident *)item->names.items[k])->name);
                    }
                } else if (item->kind == DECL_TYPE_FAMILY) {
                    add_type(scope, package, module, item->name.name);
                }
            }
            break;
        case DECL_FOREIGN:
            add_term(scope, package, module, decl->name.name);
            break;
        case DECL_PAT_SYN:
        case DECL_PAT_SYN_SIG:
            *err = unsupported("a pattern synonym", &decl->pos);
            return NULL;
        default:
            break;
        }
    }
    return scope;
}

static Child *child_new(Space space, const char *name)
{
    Child *c = (Child *)xmalloc(sizeof *c);
    c->space = space;
    c->name = name;
    return c;
}

/* Record the children of every type and class that a module declares. */
static void declared_children(Program *p, const char *package, const Module *module)
{
    size_t i;
    size_t j;
    size_t k;
    for (i = 0; i < module->decls.len; i++) {
        const Decl *decl = (const Decl *)module->decls.items[i];
        Vec *children;
        Entity parent;
        if (decl->kind != DECL_DATA && decl->kind != DECL_CLASS) {
            continue;
        }
        children = (Vec *)xmalloc(sizeof *children);
        vec_init(children);
        if (decl->kind == DECL_DATA) {
            for (j = 0; j < decl->cons.len; j++) {
                Vec names = con_children((const ConDecl *)decl->cons.items[j]);
                for (k = 0; k < names.len; k++) {
                    vec_push(children, child_new(SPACE_TERM, (const char *)names.items[k]));
                }
            }
        } else {
            for (j = 0; j < decl->body.len; j++) {
                const Decl *item = (const Decl *)decl->body.items[j];
                if (item->kind == DECL_TYPE_SIG) {
                    for (k = 0; k < item->names.len; k++) {
                        vec_push(children,
                                 child_new(SPACE_TERM,
                                           ((const Ident *)item->names.items[k])->name));
                    }
                } else if (item->kind == DECL_TYPE_FAMILY) {
                    vec_push(children, child_new(SPACE_TYPE, item->name.name));
                }
            }
        }
        parent.package = package;
        parent.module = module->name;
        parent.name = decl->name.name;
        strmap_put(&p->children, entity_key(&parent), children);
    }
}

int implicit_prelude(const Vec *extensions, const Module *module)
{
    int on = 1;
    size_t i;
    size_t j;
    Vec exts;
    vec_init(&exts);
    for (i = 0; i < extensions->len; i++) {
        vec_push(&exts, extensions->items[i]);
    }
    for (i = 0; i < module->pragmas.len; i++) {
        const char *pragma = (const char *)module->pragmas.items[i];
        Vec parts;
        const char *rest = str_starts_with(pragma, "LANGUAGE") ? pragma + 8 : "";
        parts = str_split(rest, ',');
        for (j = 0; j < parts.len; j++) {
            vec_push(&exts, str_trim((const char *)parts.items[j]));
        }
    }
    for (i = 0; i < exts.len; i++) {
        const char *ext = (const char *)exts.items[i];
        if (str_eq(ext, "ImplicitPrelude")) {
            on = 1;
        } else if (str_eq(ext, "NoImplicitPrelude") || str_eq(ext, "RebindableSyntax")) {
            on = 0;
        }
    }
    if (!on) {
        return 0;
    }
    for (i = 0; i < module->imports.len; i++) {
        if (str_eq(((const Import *)module->imports.items[i])->module, "Prelude")) {
            return 0;
        }
    }
    return 1;
}

/* The exports of a module of an earlier computation. Returns NULL and
 * sets *err when the module does not resolve or is not available. */
static const Scope *exports_of(const Program *p, const char *module, ResolveError **err)
{
    const ModuleData *data = (const ModuleData *)strmap_get(&p->modules, module);
    if (data == NULL) {
        *err = resolve_error(str_printf("resolve: module %s is not available: it is "
                                        "not vendored, or it does not parse",
                                        module),
                             NULL);
        return NULL;
    }
    if (data->exports == NULL) {
        *err = resolve_error(str_printf("module %s does not resolve: %s", module,
                                        data->exports_err->message),
                             NULL);
        return NULL;
    }
    return data->exports;
}

/* Add the children of parent that subs names, as far as source has
 * them. Returns NULL on success. */
static ResolveError *add_children(const Program *p, const Entity *parent,
                                  const Subs *subs, const Scope *source, Scope *into)
{
    const Vec *children;
    size_t i;
    if (subs->kind == SUBS_NONE) {
        return NULL;
    }
    if (subs->kind == SUBS_ALL_AND) {
        return unsupported("a bundled pattern synonym", NULL);
    }
    children = program_children(p, parent);
    if (children == NULL) {
        return NULL;
    }
    for (i = 0; i < children->len; i++) {
        const Child *child = (const Child *)children->items[i];
        void *entity;
        if (subs->kind == SUBS_SOME && !vec_has_str(&subs->names, child->name)) {
            continue;
        }
        if (child->space == SPACE_TERM) {
            entity = strmap_get(&source->terms, child->name);
            if (entity != NULL) {
                strmap_put(&into->terms, child->name, entity);
            }
        } else {
            entity = strmap_get(&source->types, child->name);
            if (entity != NULL) {
                strmap_put(&into->types, child->name, entity);
            }
        }
    }
    return NULL;
}

/* The names that one import declaration brings into scope. Returns NULL
 * and sets *err on failure. The result must not change. */
static const Scope *import_scope(const Program *p, const Import *import,
                                 ResolveError **err)
{
    const Scope *exports;
    Scope *selected;
    size_t i;
    if (import->package != NULL) {
        *err = unsupported("a package import", &import->pos);
        return NULL;
    }
    exports = exports_of(p, import->module, err);
    if (exports == NULL) {
        return NULL;
    }
    if (!import->has_items) {
        return exports;
    }
    selected = scope_new();
    for (i = 0; i < import->items.len; i++) {
        const ImportItem *item = (const ImportItem *)import->items.items[i];
        Space space;
        Entity *entity;
        if (item->kind == IMPORT_VAR || item->kind == IMPORT_PATTERN) {
            /* `pattern C` imports a constructor or a pattern synonym. */
            space = SPACE_TERM;
        } else if (item->kind == IMPORT_THING && !item->explicit_type) {
            space = SPACE_TYPE;
        } else {
            *err = unsupported("this import item", &import->pos);
            return NULL;
        }
        entity = (Entity *)strmap_get(space == SPACE_TERM ? &exports->terms
                                                          : &exports->types,
                                      item->name);
        if (entity == NULL) {
            char *what = str_printf("an import of %s, which %s does not export,",
                                    item->name, import->module);
            *err = unsupported(what, &import->pos);
            return NULL;
        }
        if (space == SPACE_TERM) {
            strmap_put(&selected->terms, item->name, entity);
        } else {
            ResolveError *e;
            strmap_put(&selected->types, item->name, entity);
            e = add_children(p, entity, &item->subs, exports, selected);
            if (e != NULL) {
                *err = e;
                return NULL;
            }
        }
    }
    if (import->hiding) {
        Scope *rest = scope_unqualified(exports);
        for (i = 0; i < selected->terms.len; i++) {
            strmap_remove(&rest->terms, selected->terms.keys[i]);
        }
        for (i = 0; i < selected->types.len; i++) {
            strmap_remove(&rest->types, selected->types.keys[i]);
        }
        return rest;
    }
    return selected;
}

/* Everything in scope at the top level of a module. Returns NULL and sets
 * *err on failure. */
static Scope *module_scope(const Program *p, const char *package,
                           const Module *module, int prelude, ResolveError **err)
{
    Scope *own = own_scope(package, module, err);
    Scope *scope;
    size_t i;
    if (own == NULL) {
        return NULL;
    }
    scope = scope_clone(own);
    /* A module's own names are also in scope with its own name as the
     * qualifier. */
    strmap_put(&scope->qualified, module->name, own);
    for (i = 0; i < module->imports.len; i++) {
        const Import *import = (const Import *)module->imports.items[i];
        const Scope *imported = import_scope(p, import, err);
        const char *qualifier;
        if (imported == NULL) {
            return NULL;
        }
        qualifier = import->alias != NULL ? import->alias : import->module;
        if (import->qualified == QUAL_NO) {
            scope_union(scope, imported);
        }
        scope_union(scope_qualified_entry(scope, qualifier), imported);
    }
    if (prelude) {
        const Scope *imported = exports_of(p, "Prelude", err);
        if (imported == NULL) {
            return NULL;
        }
        scope_union(scope, imported);
        scope_union(scope_qualified_entry(scope, "Prelude"), imported);
    }
    scope_union(scope, builtin_scope(p, module->name));
    return scope;
}

/* The names that a module exports. Returns NULL and sets *err on
 * failure. */
static Scope *module_exports(const Program *p, const char *package,
                             const Module *module, const Scope *scope,
                             ResolveError **err)
{
    Scope *out;
    size_t i;
    if (!module->has_exports) {
        return own_scope(package, module, err);
    }
    out = scope_new();
    for (i = 0; i < module->exports.len; i++) {
        const Export *export = (const Export *)module->exports.items[i];
        const Scope *source;
        Entity *entity;
        if (export->kind == EXPORT_VAR) {
            source = scope_qualifier(scope, export->name.qual);
            entity = source == NULL ? NULL
                                    : (Entity *)strmap_get(&source->terms,
                                                           export->name.name);
            if (entity != NULL) {
                strmap_put(&out->terms, export->name.name, entity);
            }
        } else if (export->kind == EXPORT_THING && !export->explicit_type) {
            ResolveError *e;
            source = scope_qualifier(scope, export->name.qual);
            if (source == NULL) {
                continue;
            }
            entity = (Entity *)strmap_get(&source->types, export->name.name);
            if (entity != NULL) {
                strmap_put(&out->types, export->name.name, entity);
                e = add_children(p, entity, &export->subs, source, out);
                if (e != NULL) {
                    *err = e;
                    return NULL;
                }
            }
        } else if (export->kind == EXPORT_MODULE) {
            /* `module M`: the names in scope both unqualified and with
             * the qualifier M, as the same entity. */
            size_t j;
            const Scope *q = scope_qualifier(scope, export->module);
            if (q == NULL) {
                continue;
            }
            for (j = 0; j < q->terms.len; j++) {
                if (strmap_get(&scope->terms, q->terms.keys[j]) == q->terms.vals[j]) {
                    strmap_put(&out->terms, q->terms.keys[j], q->terms.vals[j]);
                }
            }
            for (j = 0; j < q->types.len; j++) {
                if (strmap_get(&scope->types, q->types.keys[j]) == q->types.vals[j]) {
                    strmap_put(&out->types, q->types.keys[j], q->types.vals[j]);
                }
            }
        } else {
            *err = unsupported("this export item", &module->pos);
            return NULL;
        }
    }
    return out;
}

/* Whether a module of the package that is being added is active, or
 * imports an active module, directly or through other modules of the
 * package. */
static int reaches_active(const Program *p, const char *name, const StrMap *pending,
                          const Vec *active, Vec *seen)
{
    const SourceModule *source;
    size_t i;
    if (vec_has_str(active, name)) {
        return 1;
    }
    if (strmap_has(&p->modules, name) || vec_has_str(seen, name)) {
        return 0;
    }
    vec_push(seen, (char *)name);
    source = (const SourceModule *)strmap_get(pending, name);
    if (source == NULL) {
        return 0;
    }
    for (i = 0; i < source->module->imports.len; i++) {
        const Import *import = (const Import *)source->module->imports.items[i];
        if (reaches_active(p, import->module, pending, active, seen)) {
            return 1;
        }
    }
    return 0;
}

/* Compute the scope and the exports of a module of the package that is
 * being added, and first those of the sibling modules it imports.
 * pending holds the modules of the package by name (SourceModule *);
 * active holds the names of the modules that are being computed. */
static void compute(Program *p, const char *name, const Package *package,
                    const StrMap *pending, Vec *active)
{
    const SourceModule *source;
    const Module *module;
    ModuleData *data;
    int prelude;
    size_t i;
    if (strmap_has(&p->modules, name) || !strmap_has(pending, name)) {
        return;
    }
    if (vec_has_str(active, name)) {
        ResolveError *error = unsupported("mutually recursive modules", NULL);
        data = (ModuleData *)xmalloc(sizeof *data);
        data->scope = NULL;
        data->scope_err = error;
        data->exports = NULL;
        data->exports_err = error;
        strmap_put(&p->modules, xstrdup(name), data);
        return;
    }
    vec_push(active, (char *)name);
    source = (const SourceModule *)strmap_get(pending, name);
    module = source->module;
    prelude = implicit_prelude(&package->extensions, module);
    for (i = 0; i < module->imports.len; i++) {
        compute(p, ((const Import *)module->imports.items[i])->module, package,
                pending, active);
    }
    if (prelude) {
        compute(p, "Prelude", package, pending, active);
    }
    /* A builtin module is no import: when it is being computed, or when
     * it imports a module that is being computed, it just gives nothing
     * yet. */
    for (i = 0; i < p->builtin_modules.len; i++) {
        const char *builtin = (const char *)p->builtin_modules.items[i];
        Vec seen;
        vec_init(&seen);
        if (!reaches_active(p, builtin, pending, active, &seen)) {
            compute(p, builtin, package, pending, active);
        }
    }
    vec_pop(active);
    data = (ModuleData *)xmalloc(sizeof *data);
    data->scope_err = NULL;
    data->exports = NULL;
    data->exports_err = NULL;
    data->scope = module_scope(p, package->name, module, prelude, &data->scope_err);
    if (data->scope == NULL) {
        data->exports_err = data->scope_err;
    } else {
        data->exports = module_exports(p, package->name, module, data->scope,
                                       &data->exports_err);
    }
    strmap_put(&p->modules, xstrdup(name), data);
}

void program_add_package(Program *p, const Package *package)
{
    StrMap pending;
    Vec names;
    Vec active;
    size_t i;
    strmap_init(&pending);
    for (i = 0; i < package->modules.len; i++) {
        SourceModule *source = (SourceModule *)package->modules.items[i];
        PathEntry *entry = (PathEntry *)xmalloc(sizeof *entry);
        strmap_put(&pending, source->module->name, source);
        entry->package = package->name;
        entry->module = source->module;
        strmap_put(&p->paths, source->path, entry);
        declared_children(p, package->name, source->module);
    }
    vec_init(&names);
    for (i = 0; i < pending.len; i++) {
        vec_push(&names, pending.keys[i]);
    }
    for (i = 0; i < names.len; i++) {
        vec_init(&active);
        compute(p, (const char *)names.items[i], package, &pending, &active);
    }
}
