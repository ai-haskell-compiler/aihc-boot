/* The desugarer: syntax tree to Core. See desugar.h.
 *
 * The desugarer runs in two passes over the modules that Main reaches
 * through its imports. The first pass registers the data types, the
 * fixities and the primitives of every module, because expressions need
 * the arity of a constructor and the fixity of an operator before the
 * defining module is desugared. The second pass desugars the bindings.
 *
 * Names: scope.c gives the top-level scope of each module, so an
 * unqualified name resolves as the resolver resolves it. The desugarer
 * keeps the local scope itself, because it decides the frames and slots
 * of the local variables.
 *
 * Pattern matching compiles to nested `case` expressions. A match that
 * fails continues with a "fail" expression: the next clause of a
 * function, the next alternative of a `case`, the next guard, or a call
 * of `error`. The clauses after the first are bound in one `let`, so
 * each one is evaluated at most once per call.
 *
 * An error stops the desugarer with longjmp, so the code does not thread
 * error results through every function.
 */
#include <setjmp.h>
#include <stdlib.h>
#include <string.h>

#include "desugar.h"
#include "eval.h"
#include "fixity.h"
#include "syntax.h"
#include "tc.h"

/* --- State -------------------------------------------------------------- */

/* A local variable: its frame (numbered from the outermost, 0) and its
 * slot. */
typedef struct {
    unsigned long frame;
    unsigned long slot;
    const char *name;
} VarRef;

/* A name in the local scope. */
typedef struct {
    const char *name;
    VarRef var;
} Local;

/* A module of some package, with the package's extensions. */
typedef struct {
    const char *name;
    const char *path;
    Module *module;
    const Vec *extensions;
    int reached;
} ModSrc;

/* What a failed match continues with. */
typedef enum { FAIL_ERROR, FAIL_VAR, FAIL_NIL } FailKind;

typedef struct {
    FailKind kind;
    VarRef var;
    const char *message;
} Fail;

typedef struct {
    const Program *program;
    const Tc *tc;
    StrMap dict_cons; /* "MODULE\tCLASS" -> ConInfo *: class dictionaries */
    StrMap sources;  /* module name -> ModSrc * */
    StrMap globals;  /* "MODULE\tNAME" -> Global * */
    StrMap cons;     /* "MODULE\tNAME" -> ConInfo * */
    StrMap fixities; /* "MODULE\tNAME" -> Fixity * */
    StrMap tuples;   /* "(,)" -> ConInfo * */
    StrMap wrappers; /* "MODULE\tNAME" -> Global *: a constructor as a function */
    CoreProgram *out;
    ModSrc *main;
    /* The module being desugared. */
    const ModSrc *current;
    const Scope *scope;
    Pos pos; /* the declaration being desugared, for errors */
    unsigned long hidden; /* a counter for hidden top-level names */
    /* The local scope, innermost last, and the number of frames. */
    Local *locals;
    size_t nlocals;
    size_t cap;
    unsigned long depth;
    /* Errors. */
    jmp_buf jump;
    char *error;
} Ds;

static void fail_at(Ds *ds, const char *message)
{
    ds->error = str_printf("%s:%lu:%lu: desugar: %s", ds->current->path, ds->pos.line,
                           ds->pos.col, message);
    longjmp(ds->jump, 1);
}

static void unsupported_here(Ds *ds, const char *what)
{
    fail_at(ds, str_printf("%s is not supported yet", what));
}

static char *key(const char *module, const char *name)
{
    return str_printf("%s\t%s", module, name);
}

/* --- Locals and frames -------------------------------------------------- */

static void push_frame(Ds *ds)
{
    ds->depth++;
}

static void pop_frame(Ds *ds, size_t mark)
{
    ds->depth--;
    ds->nlocals = mark;
}

/* The innermost frame. */
static unsigned long frame_here(const Ds *ds)
{
    return ds->depth - 1;
}

static VarRef var_here(const Ds *ds, unsigned long slot, const char *name)
{
    VarRef v;
    v.frame = frame_here(ds);
    v.slot = slot;
    v.name = name;
    return v;
}

static void bind(Ds *ds, const char *name, VarRef var)
{
    if (ds->nlocals == ds->cap) {
        ds->cap = ds->cap ? ds->cap * 2 : 64;
        ds->locals = (Local *)xrealloc(ds->locals, ds->cap * sizeof *ds->locals);
    }
    ds->locals[ds->nlocals].name = name;
    ds->locals[ds->nlocals].var = var;
    ds->nlocals++;
}

static const Local *lookup_local(const Ds *ds, const char *name)
{
    size_t i = ds->nlocals;
    while (i > 0) {
        i--;
        if (str_eq(ds->locals[i].name, name)) {
            return &ds->locals[i];
        }
    }
    return NULL;
}

static Core *var_code(const Ds *ds, VarRef v)
{
    return core_var(frame_here(ds) - v.frame, v.slot, v.name);
}

/* --- Entities ----------------------------------------------------------- */

static const Entity *lookup_entity(const Ds *ds, const char *qual, const char *name)
{
    const Scope *scope;
    if (qual[0] == TC_DIRECT) {
        Entity *e = (Entity *)xmalloc(sizeof *e);
        e->package = "";
        e->module = qual + 1;
        e->name = name;
        return e;
    }
    scope = scope_qualifier(ds->scope, qual);
    if (scope == NULL) {
        return NULL;
    }
    return (const Entity *)strmap_get(&scope->terms, name);
}

static Global *global_new(Ds *ds, const char *module, const char *name)
{
    Global *g = (Global *)xcalloc(1, sizeof *g);
    g->module = module;
    g->name = name;
    g->index = ds->out->globals.len;
    vec_push(&ds->out->globals, g);
    return g;
}

/* The global for a top-level entity, created on first use. */
static Global *global_of(Ds *ds, const char *module, const char *name)
{
    char *k = key(module, name);
    Global *g = (Global *)strmap_get(&ds->globals, k);
    if (g == NULL) {
        g = global_new(ds, xstrdup(module), xstrdup(name));
        strmap_put(&ds->globals, k, g);
    }
    return g;
}

/* A global that the desugaring of a construct applies, such as `>>=` for
 * a `do` block. The boot `base` must define it. */
static Core *known_global(Ds *ds, const char *module, const char *name)
{
    return core_global(global_of(ds, module, name));
}

static ConInfo *con_new(TypeInfo *type, const char *name, int arity)
{
    ConInfo *con = (ConInfo *)xcalloc(1, sizeof *con);
    con->name = name;
    con->tag = (int)type->cons.len;
    con->arity = arity;
    con->type = type;
    vec_push(&type->cons, con);
    return con;
}

static TypeInfo *type_new_info(Ds *ds, const char *module, const char *name, int newtype)
{
    TypeInfo *t = (TypeInfo *)xcalloc(1, sizeof *t);
    t->module = module;
    t->name = name;
    t->newtype = newtype;
    vec_init(&t->cons);
    vec_push(&ds->out->types, t);
    return t;
}

/* Whether a data declaration is a box of a primitive value, such as
 * `data Int = I# Int#`: one constructor with one field, whose type is a
 * primitive type, by GHC's convention a name that ends in `#`. Int and
 * Int# then have the same representation, so the primitives of the
 * evaluator work on both. */
static int is_box(const Decl *d)
{
    const ConDecl *c;
    const Type *t;
    size_t n;
    if (d->newtype || d->cons.len != 1) {
        return 0;
    }
    c = (const ConDecl *)d->cons.items[0];
    if (c->forall.len > 0 || c->ctx != NULL) {
        return 0;
    }
    if (c->kind == CON_PREFIX && c->args.len == 1) {
        t = ((const BangType *)c->args.items[0])->ty;
    } else if (c->kind == CON_RECORD && c->fields.len == 1
               && ((const Field *)c->fields.items[0])->names.len == 1) {
        t = ((const Field *)c->fields.items[0])->ty.ty;
    } else {
        return 0;
    }
    while (t->kind == TY_APP || t->kind == TY_PAREN) {
        t = t->a;
    }
    if (t->kind != TY_CON) {
        return 0;
    }
    n = strlen(t->con.name);
    return n > 1 && t->con.name[n - 1] == '#';
}

/* The tuple constructor of n components: `()`, `(,)`, `(,,)` and so on.
 * A tuple has no module: it is syntax. */
static ConInfo *tuple_con(Ds *ds, size_t n)
{
    StrBuf b;
    char *name;
    ConInfo *con;
    size_t i;
    sb_init(&b);
    sb_putc(&b, '(');
    for (i = 1; i < n; i++) {
        sb_putc(&b, ',');
    }
    sb_putc(&b, ')');
    name = sb_take(&b);
    con = (ConInfo *)strmap_get(&ds->tuples, name);
    if (con == NULL) {
        TypeInfo *t = type_new_info(ds, "GHC.Tuple", name, 0);
        con = con_new(t, name, (int)n);
        strmap_put(&ds->tuples, name, con);
    }
    return con;
}

/* The constructor of an unboxed tuple of n components. It is a separate
 * constructor from the boxed tuple, as in the typechecker. */
static ConInfo *utuple_con(Ds *ds, size_t n)
{
    StrBuf b;
    char *name;
    ConInfo *con;
    size_t i;
    sb_init(&b);
    sb_puts(&b, "(#");
    if (n == 1) {
        sb_putc(&b, ' ');
    }
    for (i = 1; i < n; i++) {
        sb_putc(&b, ',');
    }
    sb_puts(&b, "#)");
    name = sb_take(&b);
    con = (ConInfo *)strmap_get(&ds->tuples, name);
    if (con == NULL) {
        TypeInfo *t = type_new_info(ds, "GHC.Prim", name, 0);
        con = con_new(t, name, (int)n);
        strmap_put(&ds->tuples, name, con);
    }
    return con;
}

static int is_tuple_name(const char *name)
{
    return str_eq(name, "()") || str_starts_with(name, "(,");
}

/* The constructor that a name denotes, or NULL. */
static ConInfo *con_by_name(Ds *ds, const char *qual, const char *name)
{
    const Entity *e;
    if (qual[0] == '\0' && is_tuple_name(name)) {
        return tuple_con(ds, str_eq(name, "()") ? 0 : strlen(name) - 1);
    }
    if (qual[0] == '\0' && str_eq(name, "[]")) {
        return ds->out->con_nil;
    }
    e = lookup_entity(ds, qual, name);
    if (e == NULL) {
        return NULL;
    }
    return (ConInfo *)strmap_get(&ds->cons, key(e->module, e->name));
}

static ConInfo *con_by_qname(Ds *ds, const QName *name)
{
    ConInfo *con = con_by_name(ds, name->qual, bare(name->name));
    if (con == NULL) {
        fail_at(ds, str_printf("unbound constructor %s", name->name));
    }
    return con;
}

static ConInfo *known_con(Ds *ds, const char *module, const char *name)
{
    ConInfo *con = (ConInfo *)strmap_get(&ds->cons, key(module, name));
    if (con == NULL) {
        fail_at(ds, str_printf("the boot base does not define the constructor %s.%s, "
                               "which the desugaring needs",
                               module, name));
    }
    return con;
}

/* A constructor as a function value. A nullary constructor is its own
 * value. Another one gets a wrapper `\x y -> C x y`; the constructor of a
 * newtype gets `\x -> x`. */
static Core *con_value(Ds *ds, ConInfo *con)
{
    Vec args;
    char *k;
    Global *g;
    vec_init(&args);
    if (con->arity == 0 && !con->type->newtype) {
        return core_con(con, &args);
    }
    k = key(con->type->module, con->name);
    g = (Global *)strmap_get(&ds->wrappers, k);
    if (g == NULL) {
        unsigned long i;
        g = global_new(ds, con->type->module, str_printf("%s{con}", con->name));
        if (con->type->newtype) {
            g->body = core_lam(1, core_var(0, 0, "x"));
            vec_push(&g->body->names, xstrdup("x"));
        } else {
            Core *lam;
            for (i = 0; i < (unsigned long)con->arity; i++) {
                vec_push(&args, core_var(0, i, str_printf("x%lu", i + 1)));
            }
            lam = core_lam((unsigned long)con->arity, core_con(con, &args));
            for (i = 0; i < (unsigned long)con->arity; i++) {
                vec_push(&lam->names, str_printf("x%lu", i + 1));
            }
            g->body = lam;
        }
        strmap_put(&ds->wrappers, k, g);
    }
    return core_global(g);
}

/* The value of a name in an expression: a local, a constructor or a
 * global. */
static Core *name_code(Ds *ds, const char *qual, const char *name)
{
    const Entity *e;
    ConInfo *con;
    if (qual[0] == '\0') {
        const Local *l = lookup_local(ds, name);
        if (l != NULL) {
            return var_code(ds, l->var);
        }
    }
    con = con_by_name(ds, qual, name);
    if (con != NULL) {
        return con_value(ds, con);
    }
    e = lookup_entity(ds, qual, name);
    if (e == NULL) {
        if (qual[0] != '\0') {
            fail_at(ds, str_printf("unbound name %s.%s", qual, name));
        }
        fail_at(ds, str_printf("unbound name %s", name));
    }
    return core_global(global_of(ds, e->module, e->name));
}

static ConInfo *dict_con(Ds *ds, const TcClass *cls);
static char *super_name(const TcClass *cls, size_t i);

/* The Core of a dictionary. */
static Core *ev_code(Ds *ds, TcEv *ev)
{
    Vec args;
    size_t i;
    ev = tc_ev(ev);
    switch (ev->kind) {
    case EV_VAR: {
        const Local *l = lookup_local(ds, ev->var);
        if (l == NULL) {
            fail_at(ds, str_printf("internal error: the dictionary %s is not in scope", ev->var));
        }
        return var_code(ds, l->var);
    }
    case EV_INST:
        vec_init(&args);
        for (i = 0; i < ev->args.len; i++) {
            vec_push(&args, ev_code(ds, (TcEv *)ev->args.items[i]));
        }
        return core_app(core_global(global_of(ds, ev->inst->module, ev->inst->name)), &args);
    case EV_SUPER:
        return core_app1(core_global(global_of(ds, ev->cls->module,
                                               super_name(ev->cls, (size_t)ev->index))),
                         ev_code(ds, ev->of));
    case EV_TYPEREP: {
        /* Typeable: a dictionary whose method returns the TypeRep. */
        ConInfo *rep = known_con(ds, "Data.Typeable.Internal", "TypeRep");
        Vec fields;
        Vec one;
        Core *lam;
        vec_init(&one);
        vec_push(&one, core_str(ev->rep, strlen(ev->rep)));
        lam = core_lam(1, core_con(rep, &one));
        vec_push(&lam->names, xstrdup("proxy"));
        vec_init(&fields);
        vec_push(&fields, lam);
        return core_con(dict_con(ds, ev->cls), &fields);
    }
    default:
        fail_at(ds, xstrdup("internal error: an unsolved constraint"));
        return NULL;
    }
}

/* Apply the dictionaries of a note's evs to code. */
static Core *apply_evs(Ds *ds, Core *code, const TcNote *note)
{
    Vec args;
    size_t i;
    if (note == NULL || note->evs.len == 0) {
        return code;
    }
    vec_init(&args);
    for (i = 0; i < note->evs.len; i++) {
        vec_push(&args, ev_code(ds, (TcEv *)note->evs.items[i]));
    }
    return core_app(code, &args);
}

static Core *qname_code(Ds *ds, const QName *name)
{
    return apply_evs(ds, name_code(ds, name->qual, bare(name->name)), name->note);
}

/* Whether an operator or a name in an expression is a constructor. */
static int is_con_name(const char *name)
{
    const char *n = bare(name);
    return n[0] == ':' || (n[0] >= 'A' && n[0] <= 'Z') || is_tuple_name(n)
           || str_eq(n, "[]");
}

/* --- Fail ------------------------------------------------------------- */

static Fail fail_error(const char *message)
{
    Fail f;
    memset(&f, 0, sizeof f);
    f.kind = FAIL_ERROR;
    f.message = message;
    return f;
}

static Fail fail_var(VarRef v)
{
    Fail f;
    memset(&f, 0, sizeof f);
    f.kind = FAIL_VAR;
    f.var = v;
    return f;
}

static Fail fail_nil(void)
{
    Fail f;
    memset(&f, 0, sizeof f);
    f.kind = FAIL_NIL;
    return f;
}

static Core *fail_code(Ds *ds, Fail f)
{
    Vec none;
    switch (f.kind) {
    case FAIL_VAR:
        return var_code(ds, f.var);
    case FAIL_NIL:
        vec_init(&none);
        return core_con(ds->out->con_nil, &none);
    default:
        return core_app1(known_global(ds, "GHC.Err", "error"),
                         core_str(f.message, strlen(f.message)));
    }
}

/* --- Literals ----------------------------------------------------------- */

static long parse_integer(Ds *ds, const char *raw)
{
    unsigned long value = 0;
    int base = 10;
    const char *p = raw;
    if (p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
        base = 16;
        p += 2;
    } else if (p[0] == '0' && (p[1] == 'o' || p[1] == 'O')) {
        base = 8;
        p += 2;
    } else if (p[0] == '0' && (p[1] == 'b' || p[1] == 'B')) {
        base = 2;
        p += 2;
    }
    for (; *p != '\0' && *p != '#'; p++) {
        int digit;
        if (*p == '_') {
            continue;
        }
        if (*p >= '0' && *p <= '9') {
            digit = *p - '0';
        } else if (*p >= 'a' && *p <= 'f') {
            digit = *p - 'a' + 10;
        } else if (*p >= 'A' && *p <= 'F') {
            digit = *p - 'A' + 10;
        } else {
            fail_at(ds, str_printf("cannot read the integer literal %s", raw));
            return 0;
        }
        value = value * (unsigned long)base + (unsigned long)digit;
    }
    return (long)value;
}

static Core *literal_code(Ds *ds, const Literal *lit, const TcNote *note)
{
    switch (lit->kind) {
    case LIT_INTEGER:
        if (note != NULL && note->evs.len > 0 && !tc_ev_is_int((TcEv *)note->evs.items[0])) {
            return core_app2(known_global(ds, "GHC.Num", "fromInteger"),
                             ev_code(ds, (TcEv *)note->evs.items[0]),
                             core_int(parse_integer(ds, lit->raw)));
        }
        return core_int(parse_integer(ds, lit->raw));
    case LIT_CHAR:
        return core_char(lit->ch);
    case LIT_STRING:
        if (note != NULL && note->evs.len > 0) {
            return core_app2(known_global(ds, "Data.String", "fromString"),
                             ev_code(ds, (TcEv *)note->evs.items[0]),
                             core_str(lit->value, lit->value_len));
        }
        return core_str(lit->value, lit->value_len);
    default:
        unsupported_here(ds, "a floating point literal");
        return NULL;
    }
}

/* --- Fixity resolution -------------------------------------------------- */

static Fixity op_fixity(Ds *ds, const QName *op)
{
    const char *name = bare(op->name);
    const Entity *e;
    const Fixity *f;
    if (op->qual[0] == '\0' && lookup_local(ds, name) != NULL) {
        return fixity_default();
    }
    e = lookup_entity(ds, op->qual, name);
    if (e == NULL) {
        return fixity_default();
    }
    f = (const Fixity *)strmap_get(&ds->fixities, key(e->module, e->name));
    return f != NULL ? *f : fixity_default();
}

static FNode *resolve_fixity(Ds *ds, const Vec *toks)
{
    char *err = NULL;
    FNode *n = fixity_resolve(toks, &err);
    if (n == NULL) {
        fail_at(ds, err);
    }
    return n;
}

/* --- Patterns ----------------------------------------------------------- */

/* The variables that a pattern binds, in order (char *). */
static void pattern_vars(const Pat *p, Vec *out)
{
    size_t i;
    switch (p->kind) {
    case PAT_VAR:
        vec_push(out, p->var.name);
        break;
    case PAT_AS:
        vec_push(out, p->var.name);
        pattern_vars(p->a, out);
        break;
    case PAT_CON:
    case PAT_TUPLE:
    case PAT_LIST:
        for (i = 0; i < p->args.len; i++) {
            pattern_vars((const Pat *)p->args.items[i], out);
        }
        break;
    case PAT_INFIX:
        pattern_vars(p->a, out);
        for (i = 0; i < p->steps.len; i++) {
            pattern_vars((const Pat *)((const InfixStep *)p->steps.items[i])->operand, out);
        }
        break;
    case PAT_RECORD:
        for (i = 0; i < p->fields.len; i++) {
            const FieldPat *f = (const FieldPat *)p->fields.items[i];
            if (f->pat != NULL) {
                pattern_vars(f->pat, out);
            } else {
                vec_push(out, f->name.name);
            }
        }
        break;
    case PAT_PAREN:
    case PAT_LAZY:
    case PAT_BANG:
    case PAT_VIEW:
    case PAT_SIG:
        pattern_vars(p->a, out);
        break;
    default:
        break;
    }
}

static Pat *fnode_to_pat(Ds *ds, FNode *n);

/* A pattern as a constructor applied to sub-patterns. Returns 0 for a
 * pattern that is not a constructor pattern. */
static int con_pattern(Ds *ds, const Pat *p, ConInfo **con, Vec *subs)
{
    size_t i;
    vec_init(subs);
    switch (p->kind) {
    case PAT_CON:
        *con = con_by_qname(ds, &p->con);
        for (i = 0; i < p->args.len; i++) {
            const Pat *arg = (const Pat *)p->args.items[i];
            if (arg->kind != PAT_TYPE_ARG) {
                vec_push(subs, (void *)arg);
            }
        }
        break;
    case PAT_TUPLE:
        *con = p->unboxed ? utuple_con(ds, p->args.len) : tuple_con(ds, p->args.len);
        for (i = 0; i < p->args.len; i++) {
            vec_push(subs, p->args.items[i]);
        }
        break;
    case PAT_LIST:
        if (p->args.len == 0) {
            *con = ds->out->con_nil;
        } else {
            Pat *tail = pat_new(PAT_LIST);
            for (i = 1; i < p->args.len; i++) {
                vec_push(&tail->args, p->args.items[i]);
            }
            *con = ds->out->con_cons;
            vec_push(subs, p->args.items[0]);
            vec_push(subs, tail);
        }
        break;
    case PAT_INFIX: {
        /* Resolve the chain, then build the nested constructor pattern. */
        Vec toks;
        FNode *tree;
        vec_init(&toks);
        ftok_push(&toks, FT_EXP, p->a, NULL, fixity_default());
        for (i = 0; i < p->steps.len; i++) {
            const InfixStep *step = (const InfixStep *)p->steps.items[i];
            ftok_push(&toks, FT_OP, NULL, &step->op, op_fixity(ds, &step->op));
            ftok_push(&toks, FT_EXP, step->operand, NULL, fixity_default());
        }
        tree = resolve_fixity(ds, &toks);
        return con_pattern(ds, fnode_to_pat(ds, tree), con, subs);
    }
    case PAT_RECORD: {
        /* The fields by position: a missing field is a wildcard. */
        size_t nfields;
        *con = con_by_qname(ds, &p->con);
        nfields = (size_t)((*con)->arity - (*con)->ndicts);
        for (i = 0; i < nfields; i++) {
            const char *fname = i < (*con)->field_names.len
                                    ? (const char *)(*con)->field_names.items[i]
                                    : NULL;
            Pat *sub = NULL;
            size_t j;
            for (j = 0; j < p->fields.len && fname != NULL; j++) {
                const FieldPat *f = (const FieldPat *)p->fields.items[j];
                if (str_eq(bare(f->name.name), fname)) {
                    if (f->pat != NULL) {
                        sub = f->pat;
                    } else {
                        sub = pat_new(PAT_VAR);
                        sub->var.name = (char *)fname;
                    }
                }
            }
            if (sub == NULL && p->wildcard && fname != NULL) {
                sub = pat_new(PAT_VAR);
                sub->var.name = (char *)fname;
            }
            if (sub == NULL) {
                sub = pat_new(PAT_WILDCARD);
            }
            vec_push(subs, sub);
        }
        break;
    }
    default:
        return 0;
    }
    if ((*con)->arity - (*con)->ndicts != (int)subs->len) {
        fail_at(ds, str_printf("the constructor %s takes %d arguments, but the pattern "
                               "has %lu",
                               (*con)->name, (*con)->arity, (unsigned long)subs->len));
    }
    return 1;
}

static Pat *fnode_to_pat(Ds *ds, FNode *n)
{
    Pat *p;
    switch (n->kind) {
    case FN_LEAF:
        return (Pat *)n->leaf;
    case FN_BIN:
        p = pat_new(PAT_CON);
        p->con = *n->op;
        vec_push(&p->args, fnode_to_pat(ds, n->l));
        vec_push(&p->args, fnode_to_pat(ds, n->r));
        return p;
    default:
        fail_at(ds, xstrdup("negation in a pattern chain"));
        return NULL;
    }
}

/* --- Match ---------------------------------------------------------------- */

typedef Core *(*Cont)(Ds *ds, void *ctx);

static Core *ds_expr(Ds *ds, const Expr *x);

static Core *match(Ds *ds, Pat **pats, VarRef *vars, size_t n, Cont cont, void *ctx, Fail fail);

/* The arrays for the sub-patterns of pats[0] followed by pats[1..]. */
static void splice(Pat **pats, VarRef *vars, size_t n, const Vec *subs, const VarRef *subvars,
                   Pat ***out_pats, VarRef **out_vars, size_t *out_n)
{
    size_t m = subs->len + n - 1;
    Pat **np = (Pat **)xmalloc((m + 1) * sizeof *np);
    VarRef *nv = (VarRef *)xmalloc((m + 1) * sizeof *nv);
    size_t i;
    for (i = 0; i < subs->len; i++) {
        np[i] = (Pat *)subs->items[i];
        nv[i] = subvars[i];
    }
    for (i = 1; i < n; i++) {
        np[subs->len + i - 1] = pats[i];
        nv[subs->len + i - 1] = vars[i];
    }
    *out_pats = np;
    *out_vars = nv;
    *out_n = m;
}

/* The name a sub-pattern gives its field, for printing. */
static const char *field_name(const Pat *p)
{
    while (p->kind == PAT_PAREN || p->kind == PAT_BANG || p->kind == PAT_SIG) {
        p = p->a;
    }
    if (p->kind == PAT_VAR || p->kind == PAT_AS) {
        return p->var.name;
    }
    return "_";
}

typedef struct {
    const char *name;
} VarCtx;

static Core *var_cont(Ds *ds, void *ctx)
{
    const Local *l = lookup_local(ds, ((VarCtx *)ctx)->name);
    return var_code(ds, l->var);
}

static Core *match_lazy(Ds *ds, Pat **pats, VarRef *vars, size_t n, Cont cont, void *ctx,
                        Fail fail)
{
    /* Each variable of the pattern becomes a thunk that matches on
     * demand: `let x = case v of p -> x`. */
    Vec names;
    Vec binds;
    size_t mark;
    size_t i;
    Core *body;
    Core *let;
    VarRef v = vars[0];
    vec_init(&names);
    vec_init(&binds);
    pattern_vars(pats[0]->a, &names);
    if (names.len == 0) {
        return match(ds, pats + 1, vars + 1, n - 1, cont, ctx, fail);
    }
    mark = ds->nlocals;
    push_frame(ds);
    for (i = 0; i < names.len; i++) {
        bind(ds, (const char *)names.items[i], var_here(ds, i, (const char *)names.items[i]));
    }
    for (i = 0; i < names.len; i++) {
        VarCtx vc;
        Pat *one = pats[0]->a;
        size_t inner = ds->nlocals;
        vc.name = (const char *)names.items[i];
        vec_push(&binds, match(ds, &one, &v, 1, var_cont, &vc,
                               fail_error("irrefutable pattern failed")));
        ds->nlocals = inner;
    }
    body = match(ds, pats + 1, vars + 1, n - 1, cont, ctx, fail);
    pop_frame(ds, mark);
    let = core_let(&binds, body);
    let->names = names;
    return let;
}

static Core *match(Ds *ds, Pat **pats, VarRef *vars, size_t n, Cont cont, void *ctx, Fail fail)
{
    Pat *p;
    VarRef v;
    size_t mark;
    Core *result;
    if (n == 0) {
        return cont(ds, ctx);
    }
    p = pats[0];
    v = vars[0];
    switch (p->kind) {
    case PAT_WILDCARD:
    case PAT_TYPE_ARG:
        return match(ds, pats + 1, vars + 1, n - 1, cont, ctx, fail);
    case PAT_PAREN:
    case PAT_SIG:
        pats[0] = p->a;
        return match(ds, pats, vars, n, cont, ctx, fail);
    case PAT_VAR:
        mark = ds->nlocals;
        bind(ds, p->var.name, v);
        result = match(ds, pats + 1, vars + 1, n - 1, cont, ctx, fail);
        ds->nlocals = mark;
        return result;
    case PAT_AS:
        mark = ds->nlocals;
        bind(ds, p->var.name, v);
        pats[0] = p->a;
        result = match(ds, pats, vars, n, cont, ctx, fail);
        ds->nlocals = mark;
        return result;
    case PAT_BANG: {
        Vec alts;
        vec_init(&alts);
        pats[0] = p->a;
        vec_push(&alts, alt_new(ALT_DEFAULT, match(ds, pats, vars, n, cont, ctx, fail)));
        return core_case(var_code(ds, v), &alts);
    }
    case PAT_LAZY:
        return match_lazy(ds, pats, vars, n, cont, ctx, fail);
    case PAT_LIT: {
        Vec alts;
        CoreAlt *alt;
        const TcNote *note = p->note;
        if (note != NULL && note->evs.len == 2
            && (p->lit.kind != LIT_INTEGER || !tc_ev_is_int((TcEv *)note->evs.items[1]))) {
            /* An overloaded literal: compare with (==). */
            Core *lit;
            Core *cond;
            CoreAlt *yes;
            if (p->lit.kind == LIT_INTEGER) {
                long value = parse_integer(ds, p->lit.raw);
                lit = core_app2(known_global(ds, "GHC.Num", "fromInteger"),
                                ev_code(ds, (TcEv *)note->evs.items[1]),
                                core_int(p->negative ? -value : value));
            } else if (p->lit.kind == LIT_STRING) {
                lit = core_app2(known_global(ds, "Data.String", "fromString"),
                                ev_code(ds, (TcEv *)note->evs.items[1]),
                                core_str(p->lit.value, p->lit.value_len));
            } else {
                unsupported_here(ds, "a floating point literal pattern");
                return NULL;
            }
            {
                Vec args;
                vec_init(&args);
                vec_push(&args, ev_code(ds, (TcEv *)note->evs.items[0]));
                vec_push(&args, var_code(ds, v));
                vec_push(&args, lit);
                cond = core_app(known_global(ds, "GHC.Classes", "=="), &args);
            }
            vec_init(&alts);
            yes = alt_new(ALT_CON, match(ds, pats + 1, vars + 1, n - 1, cont, ctx, fail));
            yes->con = ds->out->con_true;
            vec_push(&alts, yes);
            vec_push(&alts, alt_new(ALT_DEFAULT, fail_code(ds, fail)));
            return core_case(cond, &alts);
        }
        if (p->lit.kind == LIT_STRING) {
            /* A string pattern is a list of character patterns. */
            Pat *list = pat_new(PAT_LIST);
            const char *s = p->lit.value;
            const char *end = s + p->lit.value_len;
            while (s < end) {
                Pat *c = pat_new(PAT_LIT);
                c->lit.kind = LIT_CHAR;
                c->lit.ch = utf8_next(&s);
                vec_push(&list->args, c);
            }
            pats[0] = list;
            return match(ds, pats, vars, n, cont, ctx, fail);
        }
        vec_init(&alts);
        if (p->lit.kind == LIT_INTEGER) {
            alt = alt_new(ALT_INT, NULL);
            alt->ival = parse_integer(ds, p->lit.raw);
            if (p->negative) {
                alt->ival = -alt->ival;
            }
        } else if (p->lit.kind == LIT_CHAR) {
            alt = alt_new(ALT_CHAR, NULL);
            alt->cval = p->lit.ch;
        } else {
            unsupported_here(ds, "a floating point literal pattern");
            return NULL;
        }
        alt->body = match(ds, pats + 1, vars + 1, n - 1, cont, ctx, fail);
        vec_push(&alts, alt);
        vec_push(&alts, alt_new(ALT_DEFAULT, fail_code(ds, fail)));
        return core_case(var_code(ds, v), &alts);
    }
    case PAT_CON:
    case PAT_TUPLE:
    case PAT_LIST:
    case PAT_INFIX:
    case PAT_RECORD: {
        ConInfo *con;
        Vec subs;
        Pat **np;
        VarRef *nv;
        size_t m;
        VarRef *subvars;
        size_t i;
        Vec alts;
        CoreAlt *alt;
        con_pattern(ds, p, &con, &subs);
        subvars = (VarRef *)xmalloc((subs.len + 1) * sizeof *subvars);
        if (con->type->box) {
            /* As for a newtype, but the match evaluates the value, as a
             * match on a data constructor does. */
            Pat *bang = pat_new(PAT_BANG);
            bang->a = (Pat *)subs.items[0];
            subs.items[0] = bang;
        }
        if (con->type->newtype) {
            /* The constructor is erased: match the inner pattern on the
             * same value. */
            subvars[0] = v;
            splice(pats, vars, n, &subs, subvars, &np, &nv, &m);
            return match(ds, np, nv, m, cont, ctx, fail);
        }
        vec_init(&alts);
        alt = alt_new(ALT_CON, NULL);
        alt->con = con;
        if (con->arity > 0) {
            size_t nd = (size_t)con->ndicts;
            mark = ds->nlocals;
            push_frame(ds);
            if (nd > 0) {
                if (p->note == NULL || p->note->dicts.len != nd) {
                    fail_at(ds, xstrdup("internal error: an existential pattern without "
                                        "dictionaries"));
                }
                for (i = 0; i < nd; i++) {
                    const char *d = (const char *)p->note->dicts.items[i];
                    bind(ds, d, var_here(ds, i, d));
                    vec_push(&alt->names, (void *)d);
                }
            }
            for (i = 0; i < subs.len; i++) {
                const char *name = field_name((const Pat *)subs.items[i]);
                subvars[i] = var_here(ds, nd + i, name);
                vec_push(&alt->names, (void *)name);
            }
            splice(pats, vars, n, &subs, subvars, &np, &nv, &m);
            alt->body = match(ds, np, nv, m, cont, ctx, fail);
            pop_frame(ds, mark);
        } else {
            alt->body = match(ds, pats + 1, vars + 1, n - 1, cont, ctx, fail);
        }
        vec_push(&alts, alt);
        if (con->type->cons.len > 1) {
            vec_push(&alts, alt_new(ALT_DEFAULT, fail_code(ds, fail)));
        }
        return core_case(var_code(ds, v), &alts);
    }
    case PAT_VIEW: {
        /* `(f -> p)`: match p on `f v`. */
        Vec binds;
        Core *let;
        Core *body;
        VarRef w;
        mark = ds->nlocals;
        vec_init(&binds);
        push_frame(ds);
        vec_push(&binds, core_app1(ds_expr(ds, p->expr), var_code(ds, v)));
        w = var_here(ds, 0, "view");
        pats[0] = p->a;
        {
            VarRef *nv = (VarRef *)xmalloc((n + 1) * sizeof *nv);
            size_t i;
            for (i = 0; i < n; i++) {
                nv[i] = vars[i];
            }
            nv[0] = w;
            body = match(ds, pats, nv, n, cont, ctx, fail);
        }
        pop_frame(ds, mark);
        let = core_let(&binds, body);
        vec_push(&let->names, xstrdup("view"));
        return let;
    }
    default:
        unsupported_here(ds, "this pattern");
        return NULL;
    }
}

/* --- Declarations ------------------------------------------------------- */

static Core *ds_expr(Ds *ds, const Expr *x);
static Core *ds_rhs(Ds *ds, const Rhs *rhs, Fail fail);

/* A group of local or top-level declarations: function bindings grouped
 * by name, and pattern bindings. */
typedef struct {
    int is_pattern;
    const char *name;  /* function bindings */
    Vec clauses;       /* Decl * */
    const Decl *pat;   /* pattern bindings */
    Vec vars;          /* char *: the variables of the pattern */
    unsigned long slot; /* the slot of the function, or the hidden slot of
                           the pattern's value */
} BindGroup;

/* The groups of a declaration list (BindGroup *). At the top level, the
 * other declarations were handled by the first pass; in a local group,
 * they are errors. */
static Vec bind_groups(Ds *ds, const Vec *decls, int top)
{
    Vec groups;
    size_t i;
    vec_init(&groups);
    for (i = 0; i < decls->len; i++) {
        const Decl *d = (const Decl *)decls->items[i];
        BindGroup *g;
        const char *name;
        switch (d->kind) {
        case DECL_TYPE_SIG:
        case DECL_FIXITY:
            continue;
        case DECL_BIND:
            break;
        default:
            if (top) {
                continue;
            }
            ds->pos = d->pos;
            unsupported_here(ds, "this local declaration");
            continue;
        }
        if (d->lhs.kind == LHS_PAT) {
            g = (BindGroup *)xcalloc(1, sizeof *g);
            g->is_pattern = 1;
            g->pat = d;
            vec_init(&g->vars);
            pattern_vars(d->lhs.pat, &g->vars);
            vec_push(&groups, g);
            continue;
        }
        name = bare(d->lhs.name.name);
        if (groups.len > 0) {
            g = (BindGroup *)vec_last(&groups);
            if (!g->is_pattern && str_eq(g->name, name)) {
                vec_push(&g->clauses, (void *)d);
                continue;
            }
        }
        g = (BindGroup *)xcalloc(1, sizeof *g);
        g->name = name;
        vec_init(&g->clauses);
        vec_push(&g->clauses, (void *)d);
        vec_push(&groups, g);
    }
    return groups;
}

/* The argument patterns of a clause (Pat *). */
static Vec clause_args(const Decl *d)
{
    Vec args;
    size_t i;
    vec_init(&args);
    if (d->lhs.kind == LHS_FUN) {
        for (i = 0; i < d->lhs.args.len; i++) {
            vec_push(&args, d->lhs.args.items[i]);
        }
    } else {
        vec_push(&args, d->lhs.left);
        vec_push(&args, d->lhs.right);
        for (i = 0; i < d->lhs.args.len; i++) {
            vec_push(&args, d->lhs.args.items[i]);
        }
    }
    return args;
}

typedef struct {
    const Rhs *rhs;
    Fail fail;
} RhsCtx;

static Core *rhs_cont(Ds *ds, void *ctx)
{
    RhsCtx *c = (RhsCtx *)ctx;
    return ds_rhs(ds, c->rhs, c->fail);
}

/* One clause: match its patterns, then its right-hand side. */
static Core *ds_clause(Ds *ds, const Decl *d, VarRef *vars, Fail fail)
{
    Vec args = clause_args(d);
    Pat **pats = (Pat **)xmalloc((args.len + 1) * sizeof *pats);
    RhsCtx ctx;
    size_t i;
    ds->pos = d->pos;
    for (i = 0; i < args.len; i++) {
        pats[i] = (Pat *)args.items[i];
    }
    ctx.rhs = d->rhs;
    ctx.fail = fail;
    return match(ds, pats, vars, args.len, rhs_cont, &ctx, fail);
}

/* The clauses of one function as a lambda. */
static Core *ds_clauses_body(Ds *ds, const BindGroup *g);

/* The clauses of one function, inside the lambda of its dictionary
 * parameters. */
static Core *ds_clauses(Ds *ds, const BindGroup *g)
{
    const Decl *first = (const Decl *)g->clauses.items[0];
    const TcNote *note = first->note;
    size_t mark = ds->nlocals;
    size_t n;
    size_t i;
    Core *body;
    Core *lam;
    if (note == NULL || note->dicts.len == 0) {
        return ds_clauses_body(ds, g);
    }
    n = note->dicts.len;
    push_frame(ds);
    for (i = 0; i < n; i++) {
        const char *d = (const char *)note->dicts.items[i];
        bind(ds, d, var_here(ds, i, d));
    }
    body = ds_clauses_body(ds, g);
    pop_frame(ds, mark);
    lam = core_lam((unsigned long)n, body);
    for (i = 0; i < n; i++) {
        vec_push(&lam->names, note->dicts.items[i]);
    }
    return lam;
}

static Core *ds_clauses_body(Ds *ds, const BindGroup *g)
{
    const Decl *first = (const Decl *)g->clauses.items[0];
    size_t arity = clause_args(first).len;
    size_t i;
    size_t mark;
    VarRef *vars;
    Core *body;
    Core *lam;
    Vec first_args;
    ds->pos = first->pos;
    for (i = 1; i < g->clauses.len; i++) {
        const Decl *d = (const Decl *)g->clauses.items[i];
        if (clause_args(d).len != arity) {
            ds->pos = d->pos;
            fail_at(ds, str_printf("the clauses of %s have different numbers of "
                                   "arguments",
                                   g->name));
        }
    }
    if (arity == 0) {
        if (g->clauses.len > 1) {
            fail_at(ds, str_printf("%s is defined more than once", g->name));
        }
        return ds_rhs(ds, first->rhs, fail_error("pattern match failure"));
    }
    mark = ds->nlocals;
    push_frame(ds);
    vars = (VarRef *)xmalloc(arity * sizeof *vars);
    first_args = clause_args(first);
    for (i = 0; i < arity; i++) {
        const char *name = field_name((const Pat *)first_args.items[i]);
        if (str_eq(name, "_")) {
            name = str_printf("a%lu", (unsigned long)i + 1);
        }
        vars[i] = var_here(ds, i, name);
    }
    if (g->clauses.len == 1) {
        body = ds_clause(ds, first, vars, fail_error("pattern match failure"));
    } else {
        /* The clauses after the first are let-bound, so each one is
         * evaluated at most once: clause i + 1 is slot i. */
        Vec binds;
        size_t inner = ds->nlocals;
        Core *let;
        vec_init(&binds);
        push_frame(ds);
        for (i = 1; i < g->clauses.len; i++) {
            Fail next = i + 1 < g->clauses.len
                            ? fail_var(var_here(ds, i, "next"))
                            : fail_error("pattern match failure");
            vec_push(&binds, ds_clause(ds, (const Decl *)g->clauses.items[i], vars, next));
        }
        body = ds_clause(ds, first, vars, fail_var(var_here(ds, 0, "next")));
        pop_frame(ds, inner);
        let = core_let(&binds, body);
        for (i = 1; i < g->clauses.len; i++) {
            vec_push(&let->names, str_printf("%s{%lu}", g->name, (unsigned long)i + 1));
        }
        body = let;
    }
    pop_frame(ds, mark);
    lam = core_lam(arity, body);
    for (i = 0; i < arity; i++) {
        vec_push(&lam->names, (void *)vars[i].name);
    }
    return lam;
}

/* A pattern binding's variable: a thunk that matches the pattern on the
 * value in slot and returns the variable. */
static Core *ds_pattern_var(Ds *ds, const BindGroup *g, VarRef value, const char *name)
{
    VarCtx ctx;
    Pat *one = g->pat->lhs.pat;
    size_t mark = ds->nlocals;
    Core *result;
    ctx.name = name;
    ds->pos = g->pat->pos;
    result = match(ds, &one, &value, 1, var_cont, &ctx,
                   fail_error("irrefutable pattern failed"));
    ds->nlocals = mark;
    return result;
}

/* Start a group of local declarations: push the frame, bind the names,
 * and desugar the bindings into *binds (with their names in *names).
 * Returns 0 and pushes nothing when there are no bindings. The caller
 * desugars the body in the scope, wraps it with core_let, and calls
 * pop_frame(ds, mark). */
static int begin_decls(Ds *ds, const Vec *decls, Vec *binds, Vec *names, size_t *mark)
{
    Vec groups = bind_groups(ds, decls, 0);
    size_t i;
    size_t j;
    unsigned long slot = 0;
    *mark = ds->nlocals;
    vec_init(binds);
    vec_init(names);
    if (groups.len == 0) {
        return 0;
    }
    push_frame(ds);
    for (i = 0; i < groups.len; i++) {
        BindGroup *g = (BindGroup *)groups.items[i];
        if (g->is_pattern) {
            g->slot = slot++;
            vec_push(names, str_printf("pat{%lu}", g->slot));
            for (j = 0; j < g->vars.len; j++) {
                const char *name = (const char *)g->vars.items[j];
                bind(ds, name, var_here(ds, slot, name));
                vec_push(names, (void *)name);
                slot++;
            }
        } else {
            g->slot = slot++;
            bind(ds, g->name, var_here(ds, g->slot, g->name));
            vec_push(names, (void *)g->name);
        }
    }
    for (i = 0; i < groups.len; i++) {
        const BindGroup *g = (const BindGroup *)groups.items[i];
        if (g->is_pattern) {
            ds->pos = g->pat->pos;
            vec_push(binds, ds_rhs(ds, g->pat->rhs, fail_error("pattern match failure")));
            for (j = 0; j < g->vars.len; j++) {
                vec_push(binds, ds_pattern_var(ds, g, var_here(ds, g->slot, "pat"),
                                               (const char *)g->vars.items[j]));
            }
        } else {
            vec_push(binds, ds_clauses(ds, g));
        }
    }
    return 1;
}

static Core *finish_decls(Ds *ds, int pushed, const Vec *binds, const Vec *names, Core *body,
                          size_t mark)
{
    Core *let;
    if (!pushed) {
        return body;
    }
    pop_frame(ds, mark);
    let = core_let(binds, body);
    let->names = *names;
    return let;
}

/* --- Guards and right-hand sides ---------------------------------------- */

typedef struct {
    const GuardedRhs *g;
    size_t i;
    Fail fail;
} GuardCtx;

static Core *ds_guarded(Ds *ds, const GuardedRhs *g, size_t i, Fail fail);

static Core *guard_cont(Ds *ds, void *ctx)
{
    GuardCtx *c = (GuardCtx *)ctx;
    return ds_guarded(ds, c->g, c->i, c->fail);
}

/* The guards of one alternative from index i on, then its body. */
static Core *ds_guarded(Ds *ds, const GuardedRhs *g, size_t i, Fail fail)
{
    const Stmt *stmt;
    if (i == g->guards.len) {
        return ds_expr(ds, g->body);
    }
    stmt = (const Stmt *)g->guards.items[i];
    switch (stmt->kind) {
    case STMT_EXPR: {
        Vec alts;
        CoreAlt *yes;
        Core *cond = ds_expr(ds, stmt->expr);
        vec_init(&alts);
        yes = alt_new(ALT_CON, ds_guarded(ds, g, i + 1, fail));
        yes->con = ds->out->con_true;
        vec_push(&alts, yes);
        vec_push(&alts, alt_new(ALT_DEFAULT, fail_code(ds, fail)));
        return core_case(cond, &alts);
    }
    case STMT_BIND: {
        /* A pattern guard: bind the value, then match. */
        Vec binds;
        GuardCtx ctx;
        Pat *one = stmt->pat;
        VarRef v;
        size_t mark = ds->nlocals;
        Core *body;
        Core *let;
        vec_init(&binds);
        push_frame(ds);
        vec_push(&binds, ds_expr(ds, stmt->expr));
        v = var_here(ds, 0, "guard");
        ctx.g = g;
        ctx.i = i + 1;
        ctx.fail = fail;
        body = match(ds, &one, &v, 1, guard_cont, &ctx, fail);
        pop_frame(ds, mark);
        let = core_let(&binds, body);
        vec_push(&let->names, xstrdup("guard"));
        return let;
    }
    default: {
        Vec binds;
        Vec names;
        size_t mark;
        int pushed = begin_decls(ds, &stmt->decls, &binds, &names, &mark);
        Core *body = ds_guarded(ds, g, i + 1, fail);
        return finish_decls(ds, pushed, &binds, &names, body, mark);
    }
    }
}

/* Guarded alternatives: each one falls through to the next, and the last
 * one to fail. */
static Core *ds_guard_alts(Ds *ds, const Vec *guards, Fail fail)
{
    size_t n = guards->len;
    size_t i;
    size_t mark;
    Vec binds;
    Core *body;
    Core *let;
    if (n == 1) {
        return ds_guarded(ds, (const GuardedRhs *)guards->items[0], 0, fail);
    }
    vec_init(&binds);
    mark = ds->nlocals;
    push_frame(ds);
    for (i = 1; i < n; i++) {
        Fail next = i + 1 < n ? fail_var(var_here(ds, i, "next")) : fail;
        vec_push(&binds, ds_guarded(ds, (const GuardedRhs *)guards->items[i], 0, next));
    }
    body = ds_guarded(ds, (const GuardedRhs *)guards->items[0], 0,
                      fail_var(var_here(ds, 0, "next")));
    pop_frame(ds, mark);
    let = core_let(&binds, body);
    for (i = 1; i < n; i++) {
        vec_push(&let->names, str_printf("guard{%lu}", (unsigned long)i + 1));
    }
    return let;
}

static Core *ds_rhs(Ds *ds, const Rhs *rhs, Fail fail)
{
    Vec binds;
    Vec names;
    size_t mark;
    int pushed = begin_decls(ds, &rhs->wheres, &binds, &names, &mark);
    Core *body;
    if (!rhs->guarded) {
        body = ds_expr(ds, rhs->plain);
    } else {
        body = ds_guard_alts(ds, &rhs->guards, fail);
    }
    return finish_decls(ds, pushed, &binds, &names, body, mark);
}

/* --- Expressions -------------------------------------------------------- */

/* An operator applied to two operands. A saturated constructor becomes a
 * constructor application. */
static Core *op_apply(Ds *ds, const QName *op, Core *l, Core *r)
{
    if (is_con_name(op->name)) {
        ConInfo *con = con_by_qname(ds, op);
        if (con->arity == 2 && !con->type->newtype) {
            Vec args;
            vec_init(&args);
            vec_push(&args, l);
            vec_push(&args, r);
            return core_con(con, &args);
        }
        return core_app2(con_value(ds, con), l, r);
    }
    return core_app2(qname_code(ds, op), l, r);
}

/* Negation. neg is the EXPR_NEG expression, whose note has the Num
 * dictionary. */
static Core *negate_code(Ds *ds, const Expr *neg, Core *e)
{
    const TcNote *note = neg->note;
    if (note == NULL || note->evs.len == 0) {
        fail_at(ds, xstrdup("internal error: a negation without a Num dictionary"));
    }
    if (e->kind == CORE_INT && tc_ev_is_int((TcEv *)note->evs.items[0])) {
        e->ival = -e->ival;
        return e;
    }
    return core_app2(known_global(ds, "GHC.Num", "negate"),
                     ev_code(ds, (TcEv *)note->evs.items[0]), e);
}

static Core *fnode_code(Ds *ds, const FNode *n)
{
    switch (n->kind) {
    case FN_LEAF:
        return ds_expr(ds, (const Expr *)n->leaf);
    case FN_NEG:
        return negate_code(ds, (const Expr *)n->leaf, fnode_code(ds, n->l));
    default:
        return op_apply(ds, n->op, fnode_code(ds, n->l), fnode_code(ds, n->r));
    }
}

/* One operand of a chain, which may carry a prefix minus. */
static void push_operand(Ds *ds, Vec *toks, const Expr *e)
{
    (void)ds;
    if (e->kind == EXPR_NEG) {
        ftok_push(toks, FT_NEG, e, NULL, fixity_default());
        e = e->a;
    }
    ftok_push(toks, FT_EXP, e, NULL, fixity_default());
}

static Core *ds_infix(Ds *ds, const Expr *x)
{
    Vec toks;
    size_t i;
    vec_init(&toks);
    push_operand(ds, &toks, x->a);
    for (i = 0; i < x->steps.len; i++) {
        const InfixStep *step = (const InfixStep *)x->steps.items[i];
        ftok_push(&toks, FT_OP, NULL, &step->op, op_fixity(ds, &step->op));
        push_operand(ds, &toks, (const Expr *)step->operand);
    }
    return fnode_code(ds, resolve_fixity(ds, &toks));
}

static const Expr *strip(const Expr *e)
{
    while ((e->kind == EXPR_PAREN && (e->note == NULL || e->note->lams.len == 0))
           || e->kind == EXPR_TYPE_APP) {
        e = e->a;
    }
    return e;
}

static Core *ds_app(Ds *ds, const Expr *x)
{
    Vec args;
    Vec codes;
    const Expr *head = x;
    size_t i;
    vec_init(&args);
    vec_init(&codes);
    while (head->kind == EXPR_APP) {
        vec_insert(&args, 0, head->b);
        head = strip(head->a);
    }
    if (head->kind == EXPR_CON) {
        ConInfo *con = con_by_qname(ds, &head->name);
        const TcNote *note = head->name.note;
        size_t nevs = note != NULL ? note->evs.len : 0;
        if (con->type->newtype && args.len == 1) {
            return ds_expr(ds, (const Expr *)args.items[0]);
        }
        if (!con->type->newtype && (size_t)con->arity == args.len + nevs) {
            for (i = 0; i < nevs; i++) {
                vec_push(&codes, ev_code(ds, (TcEv *)note->evs.items[i]));
            }
            for (i = 0; i < args.len; i++) {
                vec_push(&codes, ds_expr(ds, (const Expr *)args.items[i]));
            }
            return core_con(con, &codes);
        }
    }
    for (i = 0; i < args.len; i++) {
        vec_push(&codes, ds_expr(ds, (const Expr *)args.items[i]));
    }
    return core_app(ds_expr(ds, head), &codes);
}

typedef struct {
    const Expr *body;
} ExprCtx;

static Core *expr_cont(Ds *ds, void *ctx)
{
    return ds_expr(ds, ((ExprCtx *)ctx)->body);
}

static Core *ds_lambda(Ds *ds, const Expr *x)
{
    size_t n = x->pats.len;
    size_t mark = ds->nlocals;
    VarRef *vars = (VarRef *)xmalloc((n + 1) * sizeof *vars);
    Pat **pats = (Pat **)xmalloc((n + 1) * sizeof *pats);
    ExprCtx ctx;
    Core *body;
    Core *lam;
    size_t i;
    push_frame(ds);
    for (i = 0; i < n; i++) {
        const char *name = field_name((const Pat *)x->pats.items[i]);
        if (str_eq(name, "_")) {
            name = str_printf("a%lu", (unsigned long)i + 1);
        }
        vars[i] = var_here(ds, i, name);
        pats[i] = (Pat *)x->pats.items[i];
    }
    ctx.body = x->a;
    body = match(ds, pats, vars, n, expr_cont, &ctx, fail_error("lambda pattern failed"));
    pop_frame(ds, mark);
    lam = core_lam(n, body);
    for (i = 0; i < n; i++) {
        vec_push(&lam->names, (void *)vars[i].name);
    }
    return lam;
}

/* The alternatives of a `case` on the value v. The alternatives after the
 * first are let-bound: alternative i + 1 is slot i. */
static Core *ds_alts(Ds *ds, VarRef v, const Vec *alts)
{
    size_t n = alts->len;
    size_t i;
    size_t mark;
    Vec binds;
    Core *body;
    Core *let;
    RhsCtx *ctxs;
    if (n == 0) {
        unsupported_here(ds, "an empty case");
    }
    ctxs = (RhsCtx *)xmalloc(n * sizeof *ctxs);
    if (n == 1) {
        const Alt *alt = (const Alt *)alts->items[0];
        Pat *one = alt->pat;
        ctxs[0].rhs = alt->rhs;
        ctxs[0].fail = fail_error("case: no alternative matched");
        return match(ds, &one, &v, 1, rhs_cont, &ctxs[0], ctxs[0].fail);
    }
    vec_init(&binds);
    mark = ds->nlocals;
    push_frame(ds);
    for (i = 1; i < n; i++) {
        const Alt *alt = (const Alt *)alts->items[i];
        Pat *one = alt->pat;
        size_t inner = ds->nlocals;
        ctxs[i].rhs = alt->rhs;
        ctxs[i].fail = i + 1 < n ? fail_var(var_here(ds, i, "next"))
                                 : fail_error("case: no alternative matched");
        vec_push(&binds, match(ds, &one, &v, 1, rhs_cont, &ctxs[i], ctxs[i].fail));
        ds->nlocals = inner;
    }
    {
        const Alt *alt = (const Alt *)alts->items[0];
        Pat *one = alt->pat;
        ctxs[0].rhs = alt->rhs;
        ctxs[0].fail = fail_var(var_here(ds, 0, "next"));
        body = match(ds, &one, &v, 1, rhs_cont, &ctxs[0], ctxs[0].fail);
    }
    pop_frame(ds, mark);
    let = core_let(&binds, body);
    for (i = 1; i < n; i++) {
        vec_push(&let->names, str_printf("alt{%lu}", (unsigned long)i + 1));
    }
    return let;
}

static Core *ds_case(Ds *ds, const Expr *x)
{
    Vec binds;
    size_t mark = ds->nlocals;
    Core *body;
    Core *let;
    vec_init(&binds);
    /* The scrutinee is let-bound, so the alternatives match on a
     * variable. */
    push_frame(ds);
    vec_push(&binds, ds_expr(ds, x->a));
    body = ds_alts(ds, var_here(ds, 0, "scrut"), &x->alts);
    pop_frame(ds, mark);
    let = core_let(&binds, body);
    vec_push(&let->names, xstrdup("scrut"));
    return let;
}

typedef struct {
    const Vec *stmts;
    size_t i;
    const TcNote *note;
} StmtCtx;

static Core *ds_do(Ds *ds, const Vec *stmts, size_t i, const TcNote *note);

static Core *do_cont(Ds *ds, void *ctx)
{
    StmtCtx *c = (StmtCtx *)ctx;
    return ds_do(ds, c->stmts, c->i, c->note);
}

/* A `do` block from statement i on. `>>=` and `>>` come from GHC.Base,
 * with the Monad dictionary of the note. */
static Core *ds_do(Ds *ds, const Vec *stmts, size_t i, const TcNote *note)
{
    const Stmt *stmt = (const Stmt *)stmts->items[i];
    if (i + 1 == stmts->len) {
        if (stmt->kind != STMT_EXPR) {
            fail_at(ds, xstrdup("the last statement of a do block must be an expression"));
        }
        return ds_expr(ds, stmt->expr);
    }
    switch (stmt->kind) {
    case STMT_EXPR:
        return core_app2(apply_evs(ds, known_global(ds, "GHC.Base", ">>"), note),
                         ds_expr(ds, stmt->expr), ds_do(ds, stmts, i + 1, note));
    case STMT_BIND: {
        Core *action = ds_expr(ds, stmt->expr);
        size_t mark = ds->nlocals;
        Pat *one = stmt->pat;
        VarRef v;
        StmtCtx ctx;
        Core *body;
        Core *lam;
        push_frame(ds);
        v = var_here(ds, 0, field_name(stmt->pat));
        ctx.stmts = stmts;
        ctx.i = i + 1;
        ctx.note = note;
        body = match(ds, &one, &v, 1, do_cont, &ctx, fail_error("do: pattern match failure"));
        pop_frame(ds, mark);
        lam = core_lam(1, body);
        vec_push(&lam->names, (void *)v.name);
        return core_app2(apply_evs(ds, known_global(ds, "GHC.Base", ">>="), note), action, lam);
    }
    default: {
        Vec binds;
        Vec names;
        size_t mark;
        int pushed = begin_decls(ds, &stmt->decls, &binds, &names, &mark);
        Core *body = ds_do(ds, stmts, i + 1, note);
        return finish_decls(ds, pushed, &binds, &names, body, mark);
    }
    }
}

typedef struct {
    const Expr *body;
    const Vec *stmts;
    size_t i;
} CompCtx;

static Core *ds_comp(Ds *ds, const Expr *body, const Vec *stmts, size_t i);

static Core *comp_cont(Ds *ds, void *ctx)
{
    CompCtx *c = (CompCtx *)ctx;
    return ds_comp(ds, c->body, c->stmts, c->i);
}

static Core *cons_code(Ds *ds, Core *head, Core *tail)
{
    Vec args;
    vec_init(&args);
    vec_push(&args, head);
    vec_push(&args, tail);
    return core_con(ds->out->con_cons, &args);
}

/* A list comprehension from qualifier i on, as the Haskell report
 * translates it. */
static Core *ds_comp(Ds *ds, const Expr *body, const Vec *stmts, size_t i)
{
    const Stmt *stmt;
    if (i == stmts->len) {
        return cons_code(ds, ds_expr(ds, body), fail_code(ds, fail_nil()));
    }
    stmt = (const Stmt *)stmts->items[i];
    switch (stmt->kind) {
    case STMT_EXPR: {
        Vec alts;
        CoreAlt *yes = alt_new(ALT_CON, ds_comp(ds, body, stmts, i + 1));
        vec_init(&alts);
        yes->con = ds->out->con_true;
        vec_push(&alts, yes);
        vec_push(&alts, alt_new(ALT_DEFAULT, fail_code(ds, fail_nil())));
        return core_case(ds_expr(ds, stmt->expr), &alts);
    }
    case STMT_BIND: {
        Core *list = ds_expr(ds, stmt->expr);
        size_t mark = ds->nlocals;
        Pat *one = stmt->pat;
        VarRef v;
        CompCtx ctx;
        Core *inner;
        Core *lam;
        push_frame(ds);
        v = var_here(ds, 0, field_name(stmt->pat));
        ctx.body = body;
        ctx.stmts = stmts;
        ctx.i = i + 1;
        inner = match(ds, &one, &v, 1, comp_cont, &ctx, fail_nil());
        pop_frame(ds, mark);
        lam = core_lam(1, inner);
        vec_push(&lam->names, (void *)v.name);
        return core_app2(known_global(ds, "GHC.List", "concatMap"), lam, list);
    }
    default: {
        Vec binds;
        Vec names;
        size_t mark;
        int pushed = begin_decls(ds, &stmt->decls, &binds, &names, &mark);
        Core *rest = ds_comp(ds, body, stmts, i + 1);
        return finish_decls(ds, pushed, &binds, &names, rest, mark);
    }
    }
}

static Core *ds_expr_inner(Ds *ds, const Expr *x);

static int field_index(const ConInfo *con, const char *field)
{
    size_t i;
    for (i = 0; i < con->field_names.len; i++) {
        if (str_eq((const char *)con->field_names.items[i], field)) {
            return (int)i;
        }
    }
    return -1;
}

static Core *field_value(Ds *ds, const FieldUpdate *f)
{
    if (f->value != NULL) {
        return ds_expr(ds, f->value);
    }
    {
        QName q = f->name;
        q.qual = "";
        return qname_code(ds, &q);
    }
}

/* `C { f = e }`: the fields by position. */
static Core *ds_record_con(Ds *ds, const Expr *x)
{
    ConInfo *con = con_by_qname(ds, &x->name);
    size_t nfields = (size_t)(con->arity - con->ndicts);
    Vec codes;
    size_t i;
    size_t j;
    if (x->wildcard) {
        unsupported_here(ds, "a record wildcard in an expression");
    }
    if (con->ndicts > 0) {
        unsupported_here(ds, "record construction of an existential constructor");
    }
    vec_init(&codes);
    for (i = 0; i < nfields; i++) {
        Core *code = NULL;
        for (j = 0; j < x->fields.len; j++) {
            const FieldUpdate *f = (const FieldUpdate *)x->fields.items[j];
            if (field_index(con, bare(f->name.name)) == (int)i) {
                code = field_value(ds, f);
            }
        }
        if (code == NULL) {
            code = fail_code(ds, fail_error(str_printf("Missing field in record construction %s",
                                                       i < con->field_names.len
                                                           ? (const char *)con->field_names.items[i]
                                                           : "")));
        }
        vec_push(&codes, code);
    }
    if (con->type->newtype) {
        return (Core *)codes.items[0];
    }
    return core_con(con, &codes);
}

/* `e { f = v }`: a case on the constructors that have the fields. */
static Core *ds_record_update(Ds *ds, const Expr *x)
{
    const FieldUpdate *first = (const FieldUpdate *)x->fields.items[0];
    const Entity *e = lookup_entity(ds, first->name.qual, bare(first->name.name));
    TypeInfo *type = NULL;
    size_t k = x->fields.len;
    size_t mark = ds->nlocals;
    Vec binds;
    Vec alts;
    size_t i;
    size_t j;
    Core *let;
    if (e == NULL) {
        fail_at(ds, str_printf("unbound field %s", first->name.name));
    }
    for (i = 0; i < ds->out->types.len && type == NULL; i++) {
        TypeInfo *t = (TypeInfo *)ds->out->types.items[i];
        if (!str_eq(t->module, e->module)) {
            continue;
        }
        for (j = 0; j < t->cons.len; j++) {
            if (field_index((const ConInfo *)t->cons.items[j], e->name) >= 0) {
                type = t;
            }
        }
    }
    if (type == NULL) {
        fail_at(ds, str_printf("no constructor has the field %s", first->name.name));
    }
    if (type->newtype) {
        return field_value(ds, first);
    }
    vec_init(&binds);
    push_frame(ds);
    for (i = 0; i < k; i++) {
        vec_push(&binds, field_value(ds, (const FieldUpdate *)x->fields.items[i]));
    }
    vec_push(&binds, ds_expr(ds, x->a));
    vec_init(&alts);
    for (i = 0; i < type->cons.len; i++) {
        ConInfo *con = (ConInfo *)type->cons.items[i];
        int ok = 1;
        CoreAlt *alt;
        Vec args;
        for (j = 0; j < k && ok; j++) {
            const FieldUpdate *f = (const FieldUpdate *)x->fields.items[j];
            ok = field_index(con, bare(f->name.name)) >= 0;
        }
        if (!ok || con->ndicts > 0) {
            continue;
        }
        vec_init(&args);
        for (j = 0; j < (size_t)con->arity; j++) {
            Core *arg = core_var(0, (unsigned long)j, "f");
            size_t u;
            for (u = 0; u < k; u++) {
                const FieldUpdate *f = (const FieldUpdate *)x->fields.items[u];
                if (field_index(con, bare(f->name.name)) == (int)j) {
                    arg = core_var(1, (unsigned long)u, "u");
                }
            }
            vec_push(&args, arg);
        }
        alt = alt_new(ALT_CON, core_con(con, &args));
        alt->con = con;
        for (j = 0; j < (size_t)con->arity; j++) {
            vec_push(&alt->names, (void *)"f");
        }
        vec_push(&alts, alt);
    }
    vec_push(&alts, alt_new(ALT_DEFAULT, fail_code(ds, fail_error("No match in record update"))));
    let = core_let(&binds, core_case(core_var(0, (unsigned long)k, "r"), &alts));
    pop_frame(ds, mark);
    for (i = 0; i < k; i++) {
        vec_push(&let->names, (void *)"u");
    }
    vec_push(&let->names, (void *)"r");
    return let;
}

/* An expression, inside the dictionary lambda of its note. */
static Core *ds_expr(Ds *ds, const Expr *x)
{
    size_t mark = ds->nlocals;
    size_t i;
    size_t n;
    Core *body;
    Core *lam;
    if (x->note == NULL || x->note->lams.len == 0) {
        return ds_expr_inner(ds, x);
    }
    n = x->note->lams.len;
    push_frame(ds);
    for (i = 0; i < n; i++) {
        const char *d = (const char *)x->note->lams.items[i];
        bind(ds, d, var_here(ds, i, d));
    }
    body = ds_expr_inner(ds, x);
    pop_frame(ds, mark);
    lam = core_lam((unsigned long)n, body);
    for (i = 0; i < n; i++) {
        vec_push(&lam->names, x->note->lams.items[i]);
    }
    return lam;
}

static Core *ds_expr_inner(Ds *ds, const Expr *x)
{
    size_t i;
    switch (x->kind) {
    case EXPR_VAR:
        return qname_code(ds, &x->name);
    case EXPR_CON:
        return apply_evs(ds, con_value(ds, con_by_qname(ds, &x->name)), x->name.note);
    case EXPR_LIT:
        return literal_code(ds, &x->lit, x->note);
    case EXPR_APP:
        return apply_evs(ds, ds_app(ds, x), x->note);
    case EXPR_SIG:
        return apply_evs(ds, ds_expr(ds, x->a), x->note);
    case EXPR_PAREN:
    case EXPR_TYPE_APP:
        return ds_expr(ds, x->a);
    case EXPR_INFIX:
        return ds_infix(ds, x);
    case EXPR_NEG:
        return negate_code(ds, x, ds_expr(ds, x->a));
    case EXPR_LAMBDA:
        return ds_lambda(ds, x);
    case EXPR_LAMBDA_CASE: {
        size_t mark = ds->nlocals;
        Core *body;
        Core *lam;
        push_frame(ds);
        body = ds_alts(ds, var_here(ds, 0, "scrut"), &x->alts);
        pop_frame(ds, mark);
        lam = core_lam(1, body);
        vec_push(&lam->names, xstrdup("scrut"));
        return lam;
    }
    case EXPR_LET: {
        Vec binds;
        Vec names;
        size_t mark;
        int pushed = begin_decls(ds, &x->decls, &binds, &names, &mark);
        Core *body = ds_expr(ds, x->a);
        return finish_decls(ds, pushed, &binds, &names, body, mark);
    }
    case EXPR_IF: {
        Vec alts;
        CoreAlt *yes;
        Core *cond = ds_expr(ds, x->a);
        vec_init(&alts);
        yes = alt_new(ALT_CON, ds_expr(ds, x->b));
        yes->con = ds->out->con_true;
        vec_push(&alts, yes);
        vec_push(&alts, alt_new(ALT_DEFAULT, ds_expr(ds, x->c)));
        return core_case(cond, &alts);
    }
    case EXPR_CASE:
        return ds_case(ds, x);
    case EXPR_MULTI_IF:
        return ds_guard_alts(ds, &x->guards, fail_error("multi-way if: no alternative matched"));
    case EXPR_DO:
        if (x->stmts.len == 0) {
            fail_at(ds, xstrdup("an empty do block"));
        }
        return ds_do(ds, &x->stmts, 0, x->note);
    case EXPR_TUPLE: {
        Vec items;
        vec_init(&items);
        for (i = 0; i < x->items.len; i++) {
            vec_push(&items, ds_expr(ds, (const Expr *)x->items.items[i]));
        }
        return core_con(x->unboxed ? utuple_con(ds, x->items.len) : tuple_con(ds, x->items.len),
                        &items);
    }
    case EXPR_TUPLE_SECTION: {
        /* `(, e)` is `\a -> (a, e)`. */
        Vec items;
        size_t mark = ds->nlocals;
        unsigned long missing = 0;
        Core *lam;
        vec_init(&items);
        push_frame(ds);
        for (i = 0; i < x->items.len; i++) {
            const Expr *item = (const Expr *)x->items.items[i];
            if (item == NULL) {
                vec_push(&items, var_code(ds, var_here(ds, missing, "a")));
                missing++;
            } else {
                vec_push(&items, ds_expr(ds, item));
            }
        }
        pop_frame(ds, mark);
        lam = core_lam(missing, core_con(tuple_con(ds, x->items.len), &items));
        return lam;
    }
    case EXPR_LIST: {
        Core *acc = fail_code(ds, fail_nil());
        i = x->items.len;
        while (i > 0) {
            i--;
            acc = cons_code(ds, ds_expr(ds, (const Expr *)x->items.items[i]), acc);
        }
        return acc;
    }
    case EXPR_ARITH_SEQ: {
        Vec args;
        const char *name;
        vec_init(&args);
        vec_push(&args, ds_expr(ds, x->a));
        if (x->b != NULL) {
            vec_push(&args, ds_expr(ds, x->b));
        }
        if (x->c != NULL) {
            vec_push(&args, ds_expr(ds, x->c));
        }
        name = x->b != NULL ? (x->c != NULL ? "enumFromThenTo" : "enumFromThen")
                            : (x->c != NULL ? "enumFromTo" : "enumFrom");
        return core_app(apply_evs(ds, known_global(ds, "GHC.Enum", name), x->note), &args);
    }
    case EXPR_LIST_COMP:
        return ds_comp(ds, x->a, &x->stmts, 0);
    case EXPR_LEFT_SECTION: {
        /* `(e op)` is `\y -> e op y`, which is `op e`. */
        Core *fun = is_con_name(x->name.name) ? con_value(ds, con_by_qname(ds, &x->name))
                                              : qname_code(ds, &x->name);
        return core_app1(fun, ds_expr(ds, x->a));
    }
    case EXPR_RIGHT_SECTION: {
        /* `(op e)` is `\y -> y op e`. */
        size_t mark = ds->nlocals;
        Core *r;
        Core *body;
        Core *lam;
        push_frame(ds);
        r = ds_expr(ds, x->a);
        body = op_apply(ds, &x->name, var_code(ds, var_here(ds, 0, "y")), r);
        pop_frame(ds, mark);
        lam = core_lam(1, body);
        vec_push(&lam->names, xstrdup("y"));
        return lam;
    }
    case EXPR_RECORD_CON:
        return ds_record_con(ds, x);
    case EXPR_RECORD_UPDATE:
        return ds_record_update(ds, x);
    case EXPR_HOLE:
        fail_at(ds, xstrdup("a hole `_` in an expression"));
        return NULL;
    default:
        unsupported_here(ds, "this expression");
        return NULL;
    }
}

/* --- Modules ------------------------------------------------------------ */

static void set_module(Ds *ds, const ModSrc *src)
{
    ResolveError *err = NULL;
    ds->current = src;
    ds->pos = src->module->pos;
    ds->scope = program_scope(ds->program, src->name, &err);
    if (ds->scope == NULL) {
        fail_at(ds, err != NULL ? err->message : xstrdup("the module does not resolve"));
    }
}

/* The name of a constructor declaration, and its arity. */
static const char *con_decl_name(const ConDecl *c, int *arity)
{
    size_t i;
    switch (c->kind) {
    case CON_PREFIX:
        *arity = (int)c->args.len;
        return c->name.name;
    case CON_INFIX:
        *arity = 2;
        return c->name.name;
    default:
        *arity = 0;
        for (i = 0; i < c->fields.len; i++) {
            *arity += (int)((const Field *)c->fields.items[i])->names.len;
        }
        return c->name.name;
    }
}

/* The number of class predicates in a constructor context. */
static int ctx_size(const Type *ctx)
{
    while (ctx != NULL && ctx->kind == TY_PAREN) {
        ctx = ctx->a;
    }
    if (ctx == NULL) {
        return 0;
    }
    if (ctx->kind == TY_TUPLE) {
        return (int)ctx->items.len;
    }
    if (ctx->kind == TY_CON && str_eq(ctx->con.name, "()")) {
        return 0;
    }
    return 1;
}

static ConInfo *dict_con(Ds *ds, const TcClass *cls)
{
    ConInfo *con = (ConInfo *)strmap_get(&ds->dict_cons, key(cls->module, cls->name));
    if (con == NULL) {
        fail_at(ds, str_printf("the class %s.%s has no dictionary", cls->module, cls->name));
    }
    return con;
}

static char *super_name(const TcClass *cls, size_t i)
{
    return str_printf("%s{super%lu}", cls->name, (unsigned long)i);
}

/* The first pass over a module: types, constructors, fixities,
 * primitives and class dictionaries. */
static void declare_module(Ds *ds, const ModSrc *src)
{
    const Module *m = src->module;
    const TcModuleInfo *info = ds->tc != NULL ? tc_info(ds->tc, src->name) : NULL;
    size_t i;
    size_t j;
    size_t k;
    set_module(ds, src);
    fixity_collect(&ds->fixities, src->name, m);
    for (i = 0; i < m->decls.len; i++) {
        const Decl *d = (const Decl *)m->decls.items[i];
        ds->pos = d->pos;
        switch (d->kind) {
        case DECL_DATA: {
            int box = is_box(d);
            TypeInfo *t = type_new_info(ds, src->name, d->name.name, d->newtype || box);
            t->box = box;
            for (j = 0; j < d->cons.len; j++) {
                const ConDecl *c = (const ConDecl *)d->cons.items[j];
                int arity;
                const char *name = con_decl_name(c, &arity);
                int nd = ctx_size(c->ctx);
                ConInfo *con = con_new(t, name, arity + nd);
                con->ndicts = nd;
                if (nd > 0 && d->newtype) {
                    unsupported_here(ds, "a newtype with a context");
                }
                if (c->kind == CON_RECORD) {
                    for (k = 0; k < c->fields.len; k++) {
                        const Field *f = (const Field *)c->fields.items[k];
                        size_t n;
                        for (n = 0; n < f->names.len; n++) {
                            vec_push(&con->field_names, ((const Ident *)f->names.items[n])->name);
                        }
                    }
                }
                strmap_put(&ds->cons, key(src->name, name), con);
            }
            break;
        }
        case DECL_FOREIGN: {
            const char *prim_name = d->entity != NULL ? d->entity->value : d->name.name;
            Global *g = global_of(ds, src->name, d->name.name);
            if (!str_eq(d->callconv, "prim")) {
                unsupported_here(ds, "a foreign import that is not `prim`");
            }
            g->prim = prim_lookup(prim_name);
            if (g->prim == NULL) {
                fail_at(ds, str_printf("the evaluator has no primitive named %s", prim_name));
            }
            break;
        }
        case DECL_PAT_SYN:
        case DECL_PAT_SYN_SIG:
            unsupported_here(ds, "a pattern synonym");
            break;
        default:
            break;
        }
    }
    if (info == NULL) {
        return;
    }
    for (i = 0; i < info->classes.len; i++) {
        const TcClass *cls = (const TcClass *)info->classes.items[i];
        TypeInfo *t = type_new_info(ds, src->name, str_printf("%s{dict}", cls->name), 0);
        ConInfo *con = con_new(t, str_printf("%s{Dict}", cls->name),
                               (int)(cls->supers.len + cls->methods.len));
        strmap_put(&ds->dict_cons, key(cls->module, cls->name), con);
    }
}

static Core *ds_clauses(Ds *ds, const BindGroup *g);
static Core *ev_code(Ds *ds, TcEv *ev);

/* `\d -> case d of Dict f1 .. fn -> fi`. */
static Core *field_selector(ConInfo *con, size_t index, const char *name)
{
    Vec alts;
    CoreAlt *alt;
    Core *lam;
    size_t i;
    vec_init(&alts);
    alt = alt_new(ALT_CON, core_var(0, (unsigned long)index, name));
    alt->con = con;
    for (i = 0; i < (size_t)con->arity; i++) {
        vec_push(&alt->names, i == index ? (void *)name : (void *)"_");
    }
    vec_push(&alts, alt);
    lam = core_lam(1, core_case(core_var(0, 0, "d"), &alts));
    vec_push(&lam->names, xstrdup("d"));
    return lam;
}

static BindGroup *group_of(const char *name, Vec *clauses)
{
    BindGroup *g = (BindGroup *)xcalloc(1, sizeof *g);
    g->name = name;
    g->clauses = *clauses;
    return g;
}

/* The dictionary function of an instance:
 * `\ctx1 .. ctxn -> Dict super1 .. method1 ..`. */
static Core *instance_code(Ds *ds, const TcInst *inst)
{
    const TcClass *cls = inst->cls;
    ConInfo *con = dict_con(ds, cls);
    size_t n = inst->ctx_dicts.len;
    size_t mark = ds->nlocals;
    Vec fields;
    size_t i;
    Core *body;
    Core *lam;
    if (n > 0) {
        push_frame(ds);
        for (i = 0; i < n; i++) {
            const char *d = (const char *)inst->ctx_dicts.items[i];
            bind(ds, d, var_here(ds, i, d));
        }
    }
    vec_init(&fields);
    for (i = 0; i < inst->supers.len; i++) {
        TcEv *ev = (TcEv *)inst->supers.items[i];
        vec_push(&fields, ev == NULL ? core_con(tuple_con(ds, 0), &fields) : ev_code(ds, ev));
    }
    for (i = 0; i < inst->methods.len; i++) {
        Vec *clauses = (Vec *)inst->methods.items[i];
        const TcMethod *m = (const TcMethod *)cls->methods.items[i];
        if (clauses == NULL) {
            const char *msg = str_printf("No instance nor default method for class operation %s",
                                         m->name);
            vec_push(&fields, fail_code(ds, fail_error(msg)));
        } else {
            vec_push(&fields, ds_clauses(ds, group_of(m->name, clauses)));
        }
    }
    body = core_con(con, &fields);
    if (n == 0) {
        return body;
    }
    pop_frame(ds, mark);
    lam = core_lam((unsigned long)n, body);
    for (i = 0; i < n; i++) {
        vec_push(&lam->names, inst->ctx_dicts.items[i]);
    }
    return lam;
}

/* A record field selector. */
static Core *selector_code(Ds *ds, const ModSrc *src, const TcSelector *sel)
{
    Vec alts;
    size_t i;
    Core *lam;
    vec_init(&alts);
    for (i = 0; i < sel->cons.len; i++) {
        ConInfo *con = (ConInfo *)strmap_get(&ds->cons,
                                             key(src->name, (const char *)sel->cons.items[i]));
        size_t index = (size_t)sel->indexes.items[i];
        CoreAlt *alt;
        size_t j;
        if (con == NULL) {
            fail_at(ds, str_printf("no constructor %s", (const char *)sel->cons.items[i]));
        }
        if (con->type->newtype) {
            lam = core_lam(1, core_var(0, 0, "r"));
            vec_push(&lam->names, xstrdup("r"));
            return lam;
        }
        alt = alt_new(ALT_CON, core_var(0, (unsigned long)(index + (size_t)con->ndicts), sel->name));
        alt->con = con;
        for (j = 0; j < (size_t)con->arity; j++) {
            vec_push(&alt->names, j == index ? (void *)sel->name : (void *)"_");
        }
        vec_push(&alts, alt);
    }
    {
        ConInfo *first = (ConInfo *)strmap_get(&ds->cons,
                                               key(src->name, (const char *)sel->cons.items[0]));
        if (first->type->cons.len > sel->cons.len) {
            vec_push(&alts, alt_new(ALT_DEFAULT,
                                    fail_code(ds, fail_error(str_printf(
                                                      "No match in record selector %s",
                                                      sel->name)))));
        }
    }
    lam = core_lam(1, core_case(core_var(0, 0, "r"), &alts));
    vec_push(&lam->names, xstrdup("r"));
    return lam;
}

/* The second pass: the bindings, classes, instances and selectors. */
static void define_module(Ds *ds, const ModSrc *src)
{
    const Module *m = src->module;
    const TcModuleInfo *info = ds->tc != NULL ? tc_info(ds->tc, src->name) : NULL;
    Vec groups;
    size_t i;
    size_t j;
    set_module(ds, src);
    groups = bind_groups(ds, &m->decls, 1);
    for (i = 0; i < groups.len; i++) {
        const BindGroup *g = (const BindGroup *)groups.items[i];
        if (!g->is_pattern) {
            Global *global = global_of(ds, src->name, g->name);
            global->body = ds_clauses(ds, g);
            continue;
        }
        /* A top-level pattern binding: a hidden global holds the value,
         * and each variable matches on it. */
        {
            Global *hidden = global_new(ds, src->name,
                                        str_printf("pat{%lu}", ds->hidden++));
            ds->pos = g->pat->pos;
            hidden->body = ds_rhs(ds, g->pat->rhs, fail_error("pattern match failure"));
            for (j = 0; j < g->vars.len; j++) {
                const char *name = (const char *)g->vars.items[j];
                Global *global = global_of(ds, src->name, name);
                Vec binds;
                size_t mark = ds->nlocals;
                Core *body;
                vec_init(&binds);
                push_frame(ds);
                vec_push(&binds, core_global(hidden));
                body = ds_pattern_var(ds, g, var_here(ds, 0, "pat"), name);
                pop_frame(ds, mark);
                global->body = core_let(&binds, body);
                vec_push(&global->body->names, xstrdup("pat"));
            }
        }
    }
    if (info == NULL) {
        return;
    }
    for (i = 0; i < info->classes.len; i++) {
        const TcClass *cls = (const TcClass *)info->classes.items[i];
        ConInfo *con = dict_con(ds, cls);
        for (j = 0; j < cls->supers.len; j++) {
            Global *g = global_of(ds, src->name, super_name(cls, j));
            g->body = field_selector(con, j, "super");
        }
        for (j = 0; j < cls->methods.len; j++) {
            const TcMethod *meth = (const TcMethod *)cls->methods.items[j];
            Global *g = global_of(ds, src->name, meth->name);
            g->body = field_selector(con, cls->supers.len + j, meth->name);
            if (meth->deflt != NULL) {
                Global *dm = global_of(ds, src->name, tc_default_name(meth->name));
                ds->pos = ((const Decl *)meth->deflt->items[0])->pos;
                dm->body = ds_clauses(ds, group_of(meth->name, meth->deflt));
            }
        }
    }
    for (i = 0; i < info->instances.len; i++) {
        const TcInst *inst = (const TcInst *)info->instances.items[i];
        Global *g = global_of(ds, src->name, inst->name);
        if (inst->decl != NULL) {
            ds->pos = inst->decl->pos;
        }
        g->body = instance_code(ds, inst);
    }
    for (i = 0; i < info->selectors.len; i++) {
        const TcSelector *sel = (const TcSelector *)info->selectors.items[i];
        Global *g = global_of(ds, src->name, sel->name);
        g->body = selector_code(ds, src, sel);
    }
}

/* The modules that Main reaches through imports, in no special order
 * (ModSrc *). */
static Vec reach(Ds *ds, ModSrc *main)
{
    Vec out;
    size_t i;
    vec_init(&out);
    main->reached = 1;
    vec_push(&out, main);
    for (i = 0; i < out.len; i++) {
        ModSrc *src = (ModSrc *)out.items[i];
        const Module *m = src->module;
        Vec names;
        size_t j;
        vec_init(&names);
        for (j = 0; j < m->imports.len; j++) {
            vec_push(&names, ((const Import *)m->imports.items[j])->module);
        }
        if (implicit_prelude(src->extensions, m)) {
            vec_push(&names, "Prelude");
        }
        for (j = 0; j < names.len; j++) {
            const char *name = (const char *)names.items[j];
            ModSrc *dep = (ModSrc *)strmap_get(&ds->sources, name);
            if (dep == NULL) {
                ds->current = src;
                ds->pos = m->pos;
                fail_at(ds, str_printf("the module %s is not available", name));
            }
            if (!dep->reached) {
                dep->reached = 1;
                vec_push(&out, dep);
            }
        }
    }
    return out;
}

char *desugar_program(const Program *p, const Tc *tc, const Vec *packages, CoreProgram *out)
{
    Ds ds;
    size_t i;
    size_t j;
    ModSrc *main;
    Vec reached;
    memset(&ds, 0, sizeof ds);
    ds.program = p;
    ds.tc = tc;
    strmap_init(&ds.dict_cons);
    strmap_init(&ds.sources);
    strmap_init(&ds.globals);
    strmap_init(&ds.cons);
    strmap_init(&ds.fixities);
    strmap_init(&ds.tuples);
    strmap_init(&ds.wrappers);
    ds.out = out;
    memset(out, 0, sizeof *out);
    vec_init(&out->globals);
    vec_init(&out->types);
    main = NULL;
    for (i = 0; i < packages->len; i++) {
        const Package *pkg = (const Package *)packages->items[i];
        for (j = 0; j < pkg->modules.len; j++) {
            const SourceModule *sm = (const SourceModule *)pkg->modules.items[j];
            ModSrc *src = (ModSrc *)xcalloc(1, sizeof *src);
            src->name = sm->module->name;
            src->path = sm->path;
            src->module = sm->module;
            src->extensions = &pkg->extensions;
            strmap_put(&ds.sources, src->name, src);
            if (str_eq(pkg->name, "main")) {
                main = src;
            }
        }
    }
    if (main == NULL) {
        return xstrdup("no main module");
    }
    ds.main = main;
    if (setjmp(ds.jump) != 0) {
        return ds.error;
    }
    reached = reach(&ds, ds.main);
    for (i = 0; i < reached.len && tc != NULL; i++) {
        const ModSrc *src = (const ModSrc *)reached.items[i];
        Pos pos;
        const char *err = tc_module_error(tc, src->name, &pos);
        if (err != NULL) {
            if (pos.line != 0) {
                return str_printf("%s:%lu:%lu: %s", src->path, pos.line, pos.col, err);
            }
            return str_printf("%s: %s", src->path, err);
        }
    }
    for (i = 0; i < reached.len; i++) {
        declare_module(&ds, (const ModSrc *)reached.items[i]);
    }
    /* The constructors that the desugaring and the evaluator use. */
    ds.current = ds.main;
    ds.pos = ds.main->module->pos;
    out->con_false = known_con(&ds, "GHC.Types", "False");
    out->con_true = known_con(&ds, "GHC.Types", "True");
    out->con_nil = known_con(&ds, "GHC.Types", "[]");
    out->con_cons = known_con(&ds, "GHC.Types", ":");
    /* An IO action returns `(# State# RealWorld, a #)`. */
    out->con_iores = utuple_con(&ds, 2);
    out->con_unit = tuple_con(&ds, 0);
    for (i = 0; i < reached.len; i++) {
        define_module(&ds, (const ModSrc *)reached.items[i]);
    }
    /* Every global that an expression names must have a body. */
    for (i = 0; i < out->globals.len; i++) {
        const Global *g = (const Global *)out->globals.items[i];
        if (g->body == NULL && g->prim == NULL) {
            return str_printf("%s.%s is used but not defined (the desugaring may "
                              "need it from the boot base)",
                              g->module, g->name);
        }
    }
    out->main = (Global *)strmap_get(&ds.globals, key(ds.main->name, "main"));
    if (out->main == NULL) {
        return str_printf("%s does not define main", ds.main->path);
    }
    return NULL;
}
