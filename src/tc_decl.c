/* Declarations and modules. See tc.h and tc_int.h.
 *
 * A module is checked in passes: type constructors first (so that types
 * may refer to each other in any order), then constructors, classes,
 * instances, foreign imports, the bindings, the default methods and the
 * instance methods. The solver then solves what is left, with
 * defaulting.
 */
#include <string.h>

#include "tc_int.h"

/* tc_derive.c */
void tc_derive(Tc *tc, const Decl *data, TcTyCon *tycon, const Type *cls);
void tc_add_instance(Tc *tc, const Decl *decl, const Vec *body);

/* --- Names ------------------------------------------------------------------- */

const Entity *tc_entity(Tc *tc, const QName *name, Space space)
{
    const Scope *scope;
    const char *n = bare(name->name);
    if (name->qual[0] == TC_DIRECT) {
        Entity *e = (Entity *)xmalloc(sizeof *e);
        e->package = "";
        e->module = name->qual + 1;
        e->name = n;
        return e;
    }
    if (tc->scope == NULL) {
        return NULL;
    }
    scope = scope_qualifier(tc->scope, name->qual);
    if (scope == NULL) {
        return NULL;
    }
    return (const Entity *)strmap_get(space == SPACE_TERM ? &scope->terms : &scope->types, n);
}

TcVal *tc_value(Tc *tc, const char *module, const char *name)
{
    return (TcVal *)strmap_get(&tc->values, tkey(module, name));
}

/* The name of a tuple type: `()`, `(,)`, ..., and for an unboxed tuple
 * `(# #)`, `(##)`, `(#,#)`, .... */
static char *tuple_name(size_t n, int unboxed)
{
    StrBuf b;
    size_t i;
    sb_init(&b);
    sb_puts(&b, unboxed ? "(#" : "(");
    if (unboxed && n == 1) {
        sb_putc(&b, ' ');
    }
    for (i = 1; i < n; i++) {
        sb_putc(&b, ',');
    }
    sb_puts(&b, unboxed ? "#)" : ")");
    return sb_take(&b);
}

TcTyCon *tc_tuple(Tc *tc, size_t n)
{
    return tc_tuple_of(tc, n, 0);
}

TcTyCon *tc_tuple_of(Tc *tc, size_t n, int unboxed)
{
    char *name = tuple_name(n, unboxed);
    TcTyCon *t;
    TcCon *con;
    size_t i;
    Vec fields;
    Vec none;
    t = (TcTyCon *)strmap_get(&tc->tuples, name);
    if (t != NULL) {
        return t;
    }
    t = (TcTyCon *)xcalloc(1, sizeof *t);
    t->kind = TC_DATA;
    t->module = "GHC.Tuple";
    t->name = name;
    t->arity = (int)n;
    con = (TcCon *)xcalloc(1, sizeof *con);
    con->module = "GHC.Tuple";
    con->name = name;
    con->tycon = t;
    for (i = 0; i < n; i++) {
        Ty *v = ty_bound(tc, str_printf("t%lu", (unsigned long)i + 1));
        vec_push(&t->params, v);
        vec_push(&con->univ, v);
        vec_push(&con->fields, v);
        vec_push(&con->field_names, NULL);
    }
    {
        Ty *res = ty_apps(ty_con(t), &t->params);
        i = n;
        while (i > 0) {
            i--;
            res = ty_fun(tc, (Ty *)t->params.items[i], res);
        }
        vec_init(&none);
        con->scheme = ty_forall(&con->univ, &none, res);
    }
    vec_init(&fields);
    vec_push(&t->cons, con);
    strmap_put(&tc->tuples, name, t);
    return t;
}

TcCon *tc_con(Tc *tc, const QName *name)
{
    const char *n = bare(name->name);
    const Entity *e;
    if (name->qual[0] == '\0') {
        if (str_eq(n, "()")) {
            return (TcCon *)tc_tuple(tc, 0)->cons.items[0];
        }
        if (str_starts_with(n, "(,")) {
            return (TcCon *)tc_tuple(tc, strlen(n) - 1)->cons.items[0];
        }
        if (str_eq(n, "[]")) {
            return (TcCon *)strmap_get(&tc->cons, tkey("GHC.Types", "[]"));
        }
    }
    e = tc_entity(tc, name, SPACE_TERM);
    if (e == NULL) {
        return NULL;
    }
    return (TcCon *)strmap_get(&tc->cons, tkey(e->module, e->name));
}

TcTyCon *tc_known_tycon(Tc *tc, const char *module, const char *name)
{
    return (TcTyCon *)strmap_get(&tc->tycons, tkey(module, name));
}

TcClass *tc_known_class(Tc *tc, const char *module, const char *name)
{
    return (TcClass *)strmap_get(&tc->classes, tkey(module, name));
}

Ty *tc_known_type(Tc *tc, const char *module, const char *name)
{
    TcTyCon *t = tc_known_tycon(tc, module, name);
    return t == NULL ? NULL : ty_con(t);
}

Ty *ty_list(Tc *tc, Ty *elem)
{
    TcTyCon *list = tc_known_tycon(tc, "GHC.Types", "List");
    if (list == NULL) {
        tc_fail(tc, xstrdup("the boot base does not define GHC.Types.List"));
    }
    return ty_app(ty_con(list), elem);
}

/* --- Types ---------------------------------------------------------------------- */

/* The variables that the type being converted binds: names to Ty *. */
typedef struct {
    Vec names;
    Vec types;
    int open; /* unknown variables become metas in the tyvar scope */
} ConvEnv;

static void env_init(ConvEnv *env)
{
    vec_init(&env->names);
    vec_init(&env->types);
    env->open = 0;
}

static void env_push(ConvEnv *env, const char *name, Ty *t)
{
    vec_push(&env->names, (void *)name);
    vec_push(&env->types, t);
}

static Ty *env_lookup(const ConvEnv *env, const char *name)
{
    size_t i = env->names.len;
    while (i > 0) {
        i--;
        if (str_eq((const char *)env->names.items[i], name)) {
            return (Ty *)env->types.items[i];
        }
    }
    return NULL;
}

static Ty *conv(Tc *tc, const Type *t, ConvEnv *env);
static void conv_ctx(Tc *tc, const Type *ctx, ConvEnv *env, Vec *out);
static void fill_synonym(Tc *tc, TcTyCon *syn);

static TcTyCon *tycon_of(Tc *tc, const QName *name)
{
    const char *n = bare(name->name);
    const Entity *e;
    TcTyCon *t;
    if (name->qual[0] == '\0') {
        if (str_eq(n, "()")) {
            return tc_tuple(tc, 0);
        }
        if (str_starts_with(n, "(,")) {
            return tc_tuple(tc, strlen(n) - 1);
        }
        if (str_eq(n, "[]")) {
            return tc_known_tycon(tc, "GHC.Types", "List");
        }
        if (str_eq(n, "->")) {
            return tc->tc_fun;
        }
    }
    if (name->span.start.line != 0) {
        tc->pos = name->span.start;
    }
    e = tc_entity(tc, name, SPACE_TYPE);
    if (e == NULL) {
        tc_fail(tc, str_printf("unbound type %s", name->name));
    }
    t = tc_known_tycon(tc, e->module, e->name);
    if (t == NULL) {
        tc_fail(tc, str_printf("the type %s.%s is not known to the typechecker", e->module,
                               e->name));
    }
    return t;
}

static Ty *apply_tycon(Tc *tc, TcTyCon *con, Vec *args)
{
    size_t i;
    if (con->kind == TC_CLASS) {
        tc_fail(tc, str_printf("the class %s is used as a type", con->name));
    }
    if (con->kind == TC_SYN) {
        Vec first;
        Vec rest;
        Ty *t;
        if (con->rhs == NULL) {
            fill_synonym(tc, con);
        }
        if ((int)args->len < con->arity) {
            tc_fail(tc, str_printf("the type synonym %s needs %d arguments", con->name,
                                   con->arity));
        }
        vec_init(&first);
        vec_init(&rest);
        for (i = 0; i < args->len; i++) {
            vec_push((int)i < con->arity ? &first : &rest, args->items[i]);
        }
        t = ty_subst(con->rhs, &con->params, &first);
        return ty_apps(t, &rest);
    }
    if (con->kind == TC_FAMILY) {
        Ty *f;
        Vec rest;
        if ((int)args->len < con->arity) {
            tc_fail(tc, str_printf("the type family %s needs %d arguments", con->name,
                                   con->arity));
        }
        f = ty_new(T_FAM);
        f->con = con;
        vec_init(&rest);
        for (i = 0; i < args->len; i++) {
            vec_push((int)i < con->arity ? &f->args : &rest, args->items[i]);
        }
        return ty_apps(f, &rest);
    }
    return ty_apps(ty_con(con), args);
}

static Ty *conv_var(Tc *tc, const char *name, ConvEnv *env)
{
    Ty *t = env_lookup(env, name);
    if (t != NULL) {
        return t;
    }
    t = tyvar_lookup(tc, name);
    if (t != NULL) {
        return t;
    }
    if (env->open) {
        t = ty_meta(tc);
        tyvar_push(tc, name, t);
        return t;
    }
    tc_fail(tc, str_printf("unbound type variable %s", name));
    return NULL;
}

static Ty *conv_forall(Tc *tc, const Vec *binders, const Type *body, ConvEnv *env, int scoped)
{
    size_t mark = env->names.len;
    Vec vars;
    Vec preds;
    size_t i;
    Ty *b;
    Ty *t;
    vec_init(&vars);
    vec_init(&preds);
    for (i = 0; i < binders->len; i++) {
        const TyVarBind *tv = (const TyVarBind *)binders->items[i];
        Ty *v = ty_bound(tc, tv->name.name);
        vec_push(&vars, v);
        env_push(env, tv->name.name, v);
    }
    while (body->kind == TY_PAREN) {
        body = body->a;
    }
    if (body->kind == TY_QUAL) {
        conv_ctx(tc, body->a, env, &preds);
        b = conv(tc, body->b, env);
    } else {
        b = conv(tc, body, env);
    }
    env->names.len = mark;
    env->types.len = mark;
    t = ty_forall(&vars, &preds, b);
    if (t->kind == T_FORALL) {
        t->scoped = scoped;
    }
    return t;
}

static Ty *conv(Tc *tc, const Type *t, ConvEnv *env)
{
    size_t i;
    switch (t->kind) {
    case TY_VAR:
        return conv_var(tc, t->var.name, env);
    case TY_CON: {
        Vec none;
        vec_init(&none);
        return apply_tycon(tc, tycon_of(tc, &t->con), &none);
    }
    case TY_APP: {
        Vec args;
        const Type *head;
        vec_init(&args);
        head = t;
        while (head->kind == TY_APP || head->kind == TY_APP_KIND
               || (head->kind == TY_PAREN && head->a->kind == TY_APP)) {
            if (head->kind == TY_APP) {
                vec_insert(&args, 0, conv(tc, head->b, env));
            }
            head = head->a;
        }
        while (head->kind == TY_PAREN) {
            head = head->a;
        }
        if (head->kind == TY_CON) {
            return apply_tycon(tc, tycon_of(tc, &head->con), &args);
        }
        return ty_apps(conv(tc, head, env), &args);
    }
    case TY_APP_KIND:
    case TY_KIND_SIG:
    case TY_PAREN:
        return conv(tc, t->a, env);
    case TY_FUN:
        return ty_fun(tc, conv(tc, t->a, env), conv(tc, t->b, env));
    case TY_LIST:
        return ty_list(tc, conv(tc, t->a, env));
    case TY_TUPLE: {
        Vec items;
        vec_init(&items);
        for (i = 0; i < t->items.len; i++) {
            vec_push(&items, conv(tc, (const Type *)t->items.items[i], env));
        }
        return ty_apps(ty_con(tc_tuple_of(tc, items.len, t->unboxed)), &items);
    }
    case TY_FORALL:
        return conv_forall(tc, &t->binders, t->a, env, 1);
    case TY_QUAL: {
        Vec none;
        vec_init(&none);
        return conv_forall(tc, &none, t, env, 0);
    }
    case TY_OP: {
        Vec args;
        if (str_eq(bare(t->con.name), "~")) {
            tc_fail(tc, xstrdup("an equality outside a context"));
        }
        vec_init(&args);
        vec_push(&args, conv(tc, t->a, env));
        vec_push(&args, conv(tc, t->b, env));
        return apply_tycon(tc, tycon_of(tc, &t->con), &args);
    }
    case TY_WILDCARD:
        return ty_meta(tc);
    default:
        tc_fail(tc, xstrdup("this type is not supported yet"));
        return NULL;
    }
}

static TcClass *class_of(Tc *tc, const QName *name)
{
    const Entity *e = tc_entity(tc, name, SPACE_TYPE);
    TcClass *c;
    if (name->span.start.line != 0) {
        tc->pos = name->span.start;
    }
    if (e == NULL) {
        tc_fail(tc, str_printf("unbound class %s", name->name));
    }
    c = tc_known_class(tc, e->module, e->name);
    if (c == NULL) {
        tc_fail(tc, str_printf("%s is not a class", name->name));
    }
    return c;
}

static TcPred *conv_pred(Tc *tc, const Type *t, ConvEnv *env)
{
    Vec args;
    const Type *head = t;
    TcClass *cls;
    while (head->kind == TY_PAREN) {
        head = head->a;
    }
    if (head->kind == TY_OP && str_eq(bare(head->con.name), "~")) {
        TcPred *p = pred_new(NULL, NULL);
        p->a = conv(tc, head->a, env);
        p->b = conv(tc, head->b, env);
        return p;
    }
    vec_init(&args);
    while (head->kind == TY_APP || head->kind == TY_PAREN) {
        if (head->kind == TY_APP) {
            vec_insert(&args, 0, conv(tc, head->b, env));
        }
        head = head->a;
    }
    if (head->kind != TY_CON) {
        tc_fail(tc, xstrdup("this constraint is not supported yet"));
    }
    cls = class_of(tc, &head->con);
    if ((int)args.len != cls->nparams) {
        tc_fail(tc, str_printf("the class %s takes %d arguments", cls->name, cls->nparams));
    }
    return pred_new(cls, &args);
}

static void conv_ctx(Tc *tc, const Type *ctx, ConvEnv *env, Vec *out)
{
    size_t i;
    while (ctx->kind == TY_PAREN) {
        ctx = ctx->a;
    }
    if (ctx->kind == TY_TUPLE) {
        for (i = 0; i < ctx->items.len; i++) {
            vec_push(out, conv_pred(tc, (const Type *)ctx->items.items[i], env));
        }
        return;
    }
    if (ctx->kind == TY_CON && str_eq(ctx->con.name, "()")) {
        return;
    }
    vec_push(out, conv_pred(tc, ctx, env));
}

/* The type variables of a syntax type that are free, in order of
 * appearance (char *). */
static void free_tyvars(Tc *tc, const Type *t, Vec *bound, Vec *out)
{
    size_t i;
    if (t == NULL) {
        return;
    }
    switch (t->kind) {
    case TY_VAR:
        if (!vec_has_str(bound, t->var.name) && !vec_has_str(out, t->var.name)
            && tyvar_lookup(tc, t->var.name) == NULL) {
            vec_push(out, t->var.name);
        }
        return;
    case TY_FORALL: {
        size_t mark = bound->len;
        for (i = 0; i < t->binders.len; i++) {
            vec_push(bound, ((const TyVarBind *)t->binders.items[i])->name.name);
        }
        free_tyvars(tc, t->a, bound, out);
        bound->len = mark;
        return;
    }
    case TY_TUPLE:
    case TY_PROMOTED_LIST:
        for (i = 0; i < t->items.len; i++) {
            free_tyvars(tc, (const Type *)t->items.items[i], bound, out);
        }
        return;
    default:
        free_tyvars(tc, t->a, bound, out);
        free_tyvars(tc, t->b, bound, out);
        return;
    }
}

Ty *conv_sigma(Tc *tc, const Type *t)
{
    ConvEnv env;
    Vec bound;
    Vec free;
    Vec binders;
    size_t i;
    Ty *r;
    env_init(&env);
    vec_init(&bound);
    vec_init(&free);
    free_tyvars(tc, t, &bound, &free);
    if (free.len == 0) {
        return conv(tc, t, &env);
    }
    vec_init(&binders);
    for (i = 0; i < free.len; i++) {
        TyVarBind *tv = (TyVarBind *)xcalloc(1, sizeof *tv);
        tv->name.name = (char *)free.items[i];
        vec_push(&binders, tv);
    }
    if (t->kind == TY_FORALL) {
        /* Implicit variables in a signature with an explicit forall: bind
         * them with the explicit ones, which stay scoped. */
        for (i = 0; i < t->binders.len; i++) {
            vec_push(&binders, t->binders.items[i]);
        }
        return conv_forall(tc, &binders, t->a, &env, 1);
    }
    r = conv_forall(tc, &binders, t, &env, 0);
    return r;
}

Ty *conv_open(Tc *tc, const Type *t)
{
    ConvEnv env;
    env_init(&env);
    env.open = 1;
    return conv(tc, t, &env);
}

/* A type in a declaration whose variables are given. */
static Ty *conv_with(Tc *tc, const Type *t, const Vec *names, const Vec *vars)
{
    ConvEnv env;
    size_t i;
    env_init(&env);
    for (i = 0; i < names->len; i++) {
        env_push(&env, (const char *)names->items[i], (Ty *)vars->items[i]);
    }
    return conv(tc, t, &env);
}

/* --- Type constructors ------------------------------------------------------------ */

typedef struct {
    TcTyCon *tycon;
    const Decl *decl;
    const char *module;
    const Scope *scope;
    int filling;
} SynInfo;

static StrMap syn_infos; /* "M\tN" -> SynInfo * */

static TcTyCon *tycon_new(Tc *tc, TyConKind kind, const char *name, const Vec *vars)
{
    TcTyCon *t = (TcTyCon *)xcalloc(1, sizeof *t);
    size_t i;
    t->kind = kind;
    t->module = tc->module;
    t->name = name;
    t->arity = (int)vars->len;
    for (i = 0; i < vars->len; i++) {
        const TyVarBind *tv = (const TyVarBind *)vars->items[i];
        vec_push(&t->params, ty_bound(tc, tv->name.name));
    }
    strmap_put(&tc->tycons, tkey(tc->module, name), t);
    return t;
}

static Vec binder_names(const Vec *vars)
{
    Vec names;
    size_t i;
    vec_init(&names);
    for (i = 0; i < vars->len; i++) {
        vec_push(&names, ((const TyVarBind *)vars->items[i])->name.name);
    }
    return names;
}

static void fill_synonym(Tc *tc, TcTyCon *syn)
{
    SynInfo *info = (SynInfo *)strmap_get(&syn_infos, tkey(syn->module, syn->name));
    Vec names;
    const Scope *saved = tc->scope;
    const char *saved_module = tc->module;
    if (info == NULL) {
        tc_fail(tc, str_printf("the type synonym %s has no definition", syn->name));
    }
    if (info->filling) {
        tc_fail(tc, str_printf("the type synonym %s is recursive", syn->name));
    }
    info->filling = 1;
    names = binder_names(&info->decl->vars);
    tc->scope = info->scope;
    tc->module = info->module;
    syn->rhs = conv_with(tc, info->decl->ty, &names, &syn->params);
    tc->scope = saved;
    tc->module = saved_module;
    info->filling = 0;
}

/* The constructors of a data declaration. */
static void declare_cons(Tc *tc, const Decl *d, TcTyCon *t)
{
    Vec names = binder_names(&d->vars);
    size_t i;
    size_t j;
    size_t k;
    StrMap fields; /* field name -> TcSelector * */
    strmap_init(&fields);
    t->newtype = d->newtype;
    for (i = 0; i < d->cons.len; i++) {
        const ConDecl *c = (const ConDecl *)d->cons.items[i];
        TcCon *con = (TcCon *)xcalloc(1, sizeof *con);
        ConvEnv env;
        Ty *res;
        Vec vars;
        tc->pos = c->pos;
        con->module = tc->module;
        con->tycon = t;
        con->tag = (int)i;
        con->univ = t->params;
        env_init(&env);
        for (j = 0; j < names.len; j++) {
            env_push(&env, (const char *)names.items[j], (Ty *)t->params.items[j]);
        }
        for (j = 0; j < c->forall.len; j++) {
            const TyVarBind *tv = (const TyVarBind *)c->forall.items[j];
            Ty *v = ty_bound(tc, tv->name.name);
            vec_push(&con->exist, v);
            env_push(&env, tv->name.name, v);
        }
        if (c->ctx != NULL) {
            conv_ctx(tc, c->ctx, &env, &con->ctx);
        }
        switch (c->kind) {
        case CON_PREFIX:
            con->name = c->name.name;
            for (j = 0; j < c->args.len; j++) {
                const BangType *bt = (const BangType *)c->args.items[j];
                vec_push(&con->fields, conv(tc, bt->ty, &env));
                vec_push(&con->field_names, NULL);
            }
            break;
        case CON_INFIX:
            con->name = (char *)bare(c->name.name);
            vec_push(&con->fields, conv(tc, c->left.ty, &env));
            vec_push(&con->field_names, NULL);
            vec_push(&con->fields, conv(tc, c->right.ty, &env));
            vec_push(&con->field_names, NULL);
            break;
        case CON_GADT:
            tc_fail(tc, xstrdup("a GADT constructor is not supported yet"));
            break;
        case CON_RECORD:
            con->name = c->name.name;
            for (j = 0; j < c->fields.len; j++) {
                const Field *f = (const Field *)c->fields.items[j];
                Ty *ft = conv(tc, f->ty.ty, &env);
                for (k = 0; k < f->names.len; k++) {
                    const char *fname = ((const Ident *)f->names.items[k])->name;
                    TcSelector *sel = (TcSelector *)strmap_get(&fields, fname);
                    vec_push(&con->fields, ft);
                    vec_push(&con->field_names, (void *)fname);
                    if (sel == NULL) {
                        Vec none;
                        Ty *st;
                        TcVal *v = (TcVal *)xcalloc(1, sizeof *v);
                        sel = (TcSelector *)xcalloc(1, sizeof *sel);
                        sel->name = fname;
                        sel->type_module = tc->module;
                        strmap_put(&fields, fname, sel);
                        vec_push(&tc->info->selectors, sel);
                        vec_init(&none);
                        st = ty_fun(tc, ty_apps(ty_con(t), &t->params), ft);
                        v->type = ty_forall(&t->params, &none, st);
                        strmap_put(&tc->values, tkey(tc->module, fname), v);
                    }
                    vec_push(&sel->cons, (void *)con->name);
                    vec_push(&sel->indexes, (void *)(size_t)(con->fields.len - 1));
                }
            }
            break;
        }
        res = ty_apps(ty_con(t), &t->params);
        j = con->fields.len;
        while (j > 0) {
            j--;
            res = ty_fun(tc, (Ty *)con->fields.items[j], res);
        }
        vec_init(&vars);
        for (j = 0; j < con->univ.len; j++) {
            vec_push(&vars, con->univ.items[j]);
        }
        for (j = 0; j < con->exist.len; j++) {
            vec_push(&vars, con->exist.items[j]);
        }
        con->scheme = ty_forall(&vars, &con->ctx, res);
        vec_push(&t->cons, con);
        strmap_put(&tc->cons, tkey(tc->module, con->name), con);
    }
}

/* --- Classes ------------------------------------------------------------------------ */

static TcClass *class_new(Tc *tc, const Decl *d)
{
    TcClass *c = (TcClass *)xcalloc(1, sizeof *c);
    TcTyCon *t;
    size_t i;
    c->module = tc->module;
    c->name = d->name.name;
    c->nparams = (int)d->vars.len;
    for (i = 0; i < d->vars.len; i++) {
        const TyVarBind *tv = (const TyVarBind *)d->vars.items[i];
        vec_push(&c->params, ty_bound(tc, tv->name.name));
    }
    c->typeable = str_eq(c->name, "Typeable");
    strmap_put(&tc->classes, tkey(tc->module, c->name), c);
    vec_push(&tc->info->classes, c);
    t = (TcTyCon *)xcalloc(1, sizeof *t);
    t->kind = TC_CLASS;
    t->module = tc->module;
    t->name = c->name;
    t->cls = c;
    t->arity = c->nparams;
    strmap_put(&tc->tycons, tkey(tc->module, c->name), t);
    return c;
}

static void fill_class(Tc *tc, const Decl *d, TcClass *c)
{
    Vec names = binder_names(&d->vars);
    size_t i;
    size_t j;
    tc->pos = d->pos;
    if (d->ctx != NULL) {
        ConvEnv env;
        env_init(&env);
        for (i = 0; i < names.len; i++) {
            env_push(&env, (const char *)names.items[i], (Ty *)c->params.items[i]);
        }
        conv_ctx(tc, d->ctx, &env, &c->supers);
    }
    /* Class variables are in scope in the method signatures. */
    for (i = 0; i < names.len; i++) {
        tyvar_push(tc, (const char *)names.items[i], (Ty *)c->params.items[i]);
    }
    for (i = 0; i < d->body.len; i++) {
        const Decl *item = (const Decl *)d->body.items[i];
        tc->pos = item->pos;
        if (item->kind == DECL_TYPE_SIG) {
            Ty *sig = conv_sigma(tc, item->ty);
            for (j = 0; j < item->names.len; j++) {
                TcMethod *m = (TcMethod *)xcalloc(1, sizeof *m);
                Vec preds;
                Vec args;
                TcVal *v = (TcVal *)xcalloc(1, sizeof *v);
                m->name = bare(((const Ident *)item->names.items[j])->name);
                m->sig = sig;
                vec_init(&preds);
                vec_init(&args);
                vec_push(&preds, pred_new(c, &c->params));
                m->scheme = ty_forall(&c->params, &preds, sig);
                m->scheme->scoped = 1;
                m->dm_scheme = m->scheme;
                v->type = m->scheme;
                strmap_put(&tc->values, tkey(tc->module, m->name), v);
                vec_push(&c->methods, m);
            }
        } else if (item->kind == DECL_TYPE_FAMILY) {
            TcTyCon *fam = (TcTyCon *)xcalloc(1, sizeof *fam);
            fam->kind = TC_FAMILY;
            fam->module = tc->module;
            fam->name = item->name.name;
            fam->arity = (int)item->vars.len;
            strmap_put(&tc->tycons, tkey(tc->module, fam->name), fam);
            vec_push(&c->assoc, fam);
        }
    }
    for (i = 0; i < d->body.len; i++) {
        const Decl *item = (const Decl *)d->body.items[i];
        tc->pos = item->pos;
        if (item->kind == DECL_DEFAULT_SIG) {
            const char *name = bare(item->name.name);
            for (j = 0; j < c->methods.len; j++) {
                TcMethod *m = (TcMethod *)c->methods.items[j];
                if (str_eq(m->name, name)) {
                    Vec preds;
                    vec_init(&preds);
                    vec_push(&preds, pred_new(c, &c->params));
                    m->dm_scheme = ty_forall(&c->params, &preds, conv_sigma(tc, item->ty));
                    m->dm_scheme->scoped = 1;
                }
            }
        } else if (item->kind == DECL_BIND) {
            const char *name = item->lhs.kind == LHS_PAT ? NULL : bare(item->lhs.name.name);
            if (name == NULL) {
                tc_fail(tc, xstrdup("a pattern binding in a class"));
            }
            for (j = 0; j < c->methods.len; j++) {
                TcMethod *m = (TcMethod *)c->methods.items[j];
                if (str_eq(m->name, name)) {
                    if (m->deflt == NULL) {
                        m->deflt = (Vec *)xmalloc(sizeof(Vec));
                        vec_init(m->deflt);
                    }
                    vec_push(m->deflt, (void *)item);
                }
            }
        }
    }
    for (i = 0; i < c->methods.len; i++) {
        TcMethod *m = (TcMethod *)c->methods.items[i];
        if (m->deflt != NULL) {
            TcVal *v = (TcVal *)xcalloc(1, sizeof *v);
            v->type = m->dm_scheme;
            strmap_put(&tc->values, tkey(tc->module, tc_default_name(m->name)), v);
        }
    }
    tc->tyvars.len = 0;
}

/* --- Instances --------------------------------------------------------------------- */

/* The head of an instance declaration: forall, context, class and
 * arguments. */
void tc_add_instance(Tc *tc, const Decl *decl, const Vec *body)
{
    const Type *t = decl->ty;
    const Type *ctx = NULL;
    Vec bound;
    Vec free;
    ConvEnv env;
    TcInst *inst = (TcInst *)xcalloc(1, sizeof *inst);
    Vec args;
    const Type *head;
    size_t i;
    tc->pos = decl->pos;
    if (t->kind == TY_FORALL) {
        t = t->a;
    }
    while (t->kind == TY_PAREN) {
        t = t->a;
    }
    if (t->kind == TY_QUAL) {
        ctx = t->a;
        t = t->b;
    }
    vec_init(&bound);
    vec_init(&free);
    free_tyvars(tc, decl->ty, &bound, &free);
    env_init(&env);
    for (i = 0; i < free.len; i++) {
        Ty *v = ty_bound(tc, (const char *)free.items[i]);
        vec_push(&inst->vars, v);
        env_push(&env, (const char *)free.items[i], v);
    }
    if (decl->ty->kind == TY_FORALL) {
        for (i = 0; i < decl->ty->binders.len; i++) {
            const TyVarBind *tv = (const TyVarBind *)decl->ty->binders.items[i];
            Ty *v = ty_bound(tc, tv->name.name);
            vec_push(&inst->vars, v);
            env_push(&env, tv->name.name, v);
        }
    }
    vec_init(&args);
    head = t;
    while (head->kind == TY_APP || head->kind == TY_PAREN) {
        if (head->kind == TY_APP) {
            vec_insert(&args, 0, (void *)head->b);
        }
        head = head->a;
    }
    if (head->kind != TY_CON) {
        tc_fail(tc, xstrdup("this instance head is not supported yet"));
    }
    inst->cls = class_of(tc, &head->con);
    if ((int)args.len != inst->cls->nparams) {
        tc_fail(tc, str_printf("the class %s takes %d arguments", inst->cls->name,
                               inst->cls->nparams));
    }
    for (i = 0; i < args.len; i++) {
        vec_push(&inst->head, conv(tc, (const Type *)args.items[i], &env));
    }
    if (ctx != NULL) {
        conv_ctx(tc, ctx, &env, &inst->ctx);
    }
    inst->module = tc->module;
    {
        StrBuf b;
        sb_init(&b);
        sb_printf(&b, "{%s", inst->cls->name);
        for (i = 0; i < inst->head.len; i++) {
            sb_putc(&b, ' ');
            sb_puts(&b, ty_str_atom((Ty *)inst->head.items[i]));
        }
        sb_putc(&b, '}');
        inst->name = sb_take(&b);
    }
    inst->path = tc->path;
    inst->decl = decl;
    inst->body = *body;
    vec_push(&inst->cls->instances, inst);
    vec_push(&tc->info->instances, inst);
    /* Associated type instances. */
    for (i = 0; i < body->len; i++) {
        const Decl *item = (const Decl *)body->items[i];
        if (item->kind == DECL_TYPE_INSTANCE) {
            const Type *lhs = item->lhs_ty;
            Vec largs;
            TcTyCon *fam;
            TcFamEqn *eq = (TcFamEqn *)xcalloc(1, sizeof *eq);
            size_t j;
            vec_init(&largs);
            while (lhs->kind == TY_APP || lhs->kind == TY_PAREN) {
                if (lhs->kind == TY_APP) {
                    vec_insert(&largs, 0, (void *)lhs->b);
                }
                lhs = lhs->a;
            }
            fam = tycon_of(tc, &lhs->con);
            if (fam->kind != TC_FAMILY) {
                tc_fail(tc, xstrdup("a type instance of a type that is not a family"));
            }
            eq->vars = inst->vars;
            for (j = 0; j < largs.len; j++) {
                vec_push(&eq->lhs, conv(tc, (const Type *)largs.items[j], &env));
            }
            eq->rhs = conv(tc, item->ty, &env);
            vec_push(&fam->eqns, eq);
        }
    }
}

/* A top-level `type instance`. */
static void type_instance(Tc *tc, const Decl *item)
{
    const Type *lhs = item->lhs_ty;
    Vec largs;
    Vec bound;
    Vec free;
    ConvEnv env;
    TcTyCon *fam;
    TcFamEqn *eq = (TcFamEqn *)xcalloc(1, sizeof *eq);
    size_t j;
    vec_init(&bound);
    vec_init(&free);
    free_tyvars(tc, item->lhs_ty, &bound, &free);
    env_init(&env);
    for (j = 0; j < free.len; j++) {
        Ty *v = ty_bound(tc, (const char *)free.items[j]);
        vec_push(&eq->vars, v);
        env_push(&env, (const char *)free.items[j], v);
    }
    vec_init(&largs);
    while (lhs->kind == TY_APP || lhs->kind == TY_PAREN) {
        if (lhs->kind == TY_APP) {
            vec_insert(&largs, 0, (void *)lhs->b);
        }
        lhs = lhs->a;
    }
    fam = tycon_of(tc, &lhs->con);
    if (fam->kind != TC_FAMILY) {
        tc_fail(tc, xstrdup("a type instance of a type that is not a family"));
    }
    for (j = 0; j < largs.len; j++) {
        vec_push(&eq->lhs, conv(tc, (const Type *)largs.items[j], &env));
    }
    eq->rhs = conv(tc, item->ty, &env);
    vec_push(&fam->eqns, eq);
}

/* Check the methods of an instance. */
static void check_instance(Tc *tc, TcInst *inst)
{
    TcClass *c = inst->cls;
    Vec sk;
    Vec ctx;
    Vec head;
    size_t i;
    size_t j;
    size_t ntyvars = tc->tyvars.len;
    tc->path = inst->path;
    if (inst->decl != NULL) {
        tc->pos = inst->decl->pos;
    }
    tc->level++;
    vec_init(&sk);
    for (i = 0; i < inst->vars.len; i++) {
        Ty *s = ty_skol(tc, ((Ty *)inst->vars.items[i])->name);
        vec_push(&sk, s);
        if (tc->scoped_tyvars) {
            tyvar_push(tc, s->name, s);
        }
    }
    vec_init(&ctx);
    for (i = 0; i < inst->ctx.len; i++) {
        vec_push(&ctx, pred_subst((const TcPred *)inst->ctx.items[i], &inst->vars, &sk));
    }
    vec_init(&head);
    for (i = 0; i < inst->head.len; i++) {
        vec_push(&head, ty_subst((Ty *)inst->head.items[i], &inst->vars, &sk));
    }
    push_givens(tc, &ctx, &inst->ctx_dicts);
    for (i = 0; i < c->supers.len; i++) {
        TcPred *sp = pred_subst((const TcPred *)c->supers.items[i], &c->params, &head);
        if (sp->cls == NULL) {
            unify(tc, sp->a, sp->b);
            vec_push(&inst->supers, NULL);
        } else {
            vec_push(&inst->supers, emit_wanted(tc, sp));
        }
    }
    for (i = 0; i < c->methods.len; i++) {
        TcMethod *m = (TcMethod *)c->methods.items[i];
        Vec *clauses = (Vec *)xmalloc(sizeof *clauses);
        Ty *sigma;
        vec_init(clauses);
        for (j = 0; j < inst->body.len; j++) {
            const Decl *d = (const Decl *)inst->body.items[j];
            if (d->kind == DECL_BIND && d->lhs.kind != LHS_PAT
                && str_eq(bare(d->lhs.name.name), m->name)) {
                vec_push(clauses, (void *)d);
            }
        }
        if (clauses->len == 0 && m->deflt != NULL) {
            /* The default: `m = C.m{default}`. */
            Decl *d = decl_new(DECL_BIND, inst->decl != NULL ? inst->decl->pos : tc->pos);
            Expr *e = expr_new(EXPR_VAR);
            d->lhs.kind = LHS_FUN;
            d->lhs.name.name = (char *)m->name;
            d->lhs.name.span.start = d->pos;
            vec_init(&d->lhs.args);
            d->rhs = (Rhs *)xcalloc(1, sizeof(Rhs));
            e->name.qual = str_printf("%c%s", TC_DIRECT, c->module);
            e->name.name = tc_default_name(m->name);
            d->rhs->plain = e;
            vec_push(clauses, d);
        }
        if (clauses->len == 0) {
            vec_push(&inst->methods, NULL);
            continue;
        }
        sigma = ty_subst(m->sig, &c->params, &head);
        tc_check_binding(tc, clauses, sigma, m->name);
        vec_push(&inst->methods, clauses);
    }
    for (j = 0; j < inst->body.len; j++) {
        const Decl *d = (const Decl *)inst->body.items[j];
        if (d->kind == DECL_BIND) {
            int found = 0;
            const char *name = d->lhs.kind == LHS_PAT ? "" : bare(d->lhs.name.name);
            for (i = 0; i < c->methods.len; i++) {
                if (str_eq(((TcMethod *)c->methods.items[i])->name, name)) {
                    found = 1;
                }
            }
            if (!found) {
                tc->pos = d->pos;
                tc_fail(tc, str_printf("%s is not a method of the class %s", name, c->name));
            }
        }
    }
    solve(tc);
    pop_givens(tc);
    tc->tyvars.len = ntyvars;
    tc->level--;
}

static void check_defaults(Tc *tc, TcClass *c)
{
    size_t i;
    for (i = 0; i < c->methods.len; i++) {
        TcMethod *m = (TcMethod *)c->methods.items[i];
        if (m->deflt != NULL) {
            tc->pos = ((const Decl *)m->deflt->items[0])->pos;
            tc_check_binding(tc, m->deflt, m->dm_scheme, m->name);
        }
    }
}

/* --- Modules ------------------------------------------------------------------------ */

static int has_extension(const Vec *exts, const Module *m, const char *name)
{
    int on = 0;
    size_t i;
    size_t j;
    char *no = str_concat("No", name);
    for (i = 0; i < exts->len; i++) {
        const char *e = (const char *)exts->items[i];
        if (str_eq(e, name)) {
            on = 1;
        } else if (str_eq(e, no)) {
            on = 0;
        }
    }
    for (i = 0; i < m->pragmas.len; i++) {
        const char *pragma = (const char *)m->pragmas.items[i];
        Vec parts;
        const char *rest = str_starts_with(pragma, "LANGUAGE") ? pragma + 8 : "";
        parts = str_split(rest, ',');
        for (j = 0; j < parts.len; j++) {
            char *e = str_trim((const char *)parts.items[j]);
            if (str_eq(e, name)) {
                on = 1;
            } else if (str_eq(e, no)) {
                on = 0;
            }
        }
    }
    return on;
}

static void check_module(Tc *tc, const char *path, const Module *m, const Vec *exts)
{
    size_t i;
    size_t j;
    Vec classes;
    Vec class_decls;
    Vec datas;
    Vec data_decls;
    ResolveError *rerr = NULL;
    tc->module = m->name;
    tc->path = path;
    tc->scope = program_scope(tc->program, m->name, &rerr);
    tc->pos = m->pos;
    tc->level = 0;
    vec_init(&tc->wanted);
    tc->givens = NULL;
    tc->nlocals = 0;
    vec_init(&tc->tyvars);
    tc->overloaded_strings = has_extension(exts, m, "OverloadedStrings");
    tc->scoped_tyvars = has_extension(exts, m, "ScopedTypeVariables");
    tc->info = (TcModuleInfo *)xcalloc(1, sizeof *tc->info);
    strmap_put(&tc->infos, m->name, tc->info);
    if (tc->scope == NULL) {
        tc_fail(tc, rerr != NULL ? rerr->message : xstrdup("the module does not resolve"));
    }
    fixity_collect(&tc->fixities, m->name, m);
    /* Type constructors and classes. */
    vec_init(&classes);
    vec_init(&class_decls);
    vec_init(&datas);
    vec_init(&data_decls);
    for (i = 0; i < m->decls.len; i++) {
        const Decl *d = (const Decl *)m->decls.items[i];
        tc->pos = d->pos;
        switch (d->kind) {
        case DECL_DATA:
            vec_push(&datas, tycon_new(tc, TC_DATA, d->name.name, &d->vars));
            vec_push(&data_decls, (void *)d);
            break;
        case DECL_TYPE_SYN: {
            TcTyCon *t = tycon_new(tc, TC_SYN, d->name.name, &d->vars);
            SynInfo *info = (SynInfo *)xcalloc(1, sizeof *info);
            info->tycon = t;
            info->decl = d;
            info->module = tc->module;
            info->scope = tc->scope;
            strmap_put(&syn_infos, tkey(tc->module, d->name.name), info);
            break;
        }
        case DECL_TYPE_FAMILY:
            tycon_new(tc, TC_FAMILY, d->name.name, &d->vars);
            break;
        case DECL_CLASS:
            vec_push(&classes, class_new(tc, d));
            vec_push(&class_decls, (void *)d);
            break;
        default:
            break;
        }
    }
    for (i = 0; i < m->decls.len; i++) {
        const Decl *d = (const Decl *)m->decls.items[i];
        if (d->kind == DECL_TYPE_SYN) {
            TcTyCon *t = tc_known_tycon(tc, tc->module, d->name.name);
            tc->pos = d->pos;
            if (t->rhs == NULL) {
                fill_synonym(tc, t);
            }
        }
    }
    for (i = 0; i < classes.len; i++) {
        fill_class(tc, (const Decl *)class_decls.items[i], (TcClass *)classes.items[i]);
    }
    for (i = 0; i < datas.len; i++) {
        declare_cons(tc, (const Decl *)data_decls.items[i], (TcTyCon *)datas.items[i]);
    }
    /* Instances. */
    for (i = 0; i < m->decls.len; i++) {
        const Decl *d = (const Decl *)m->decls.items[i];
        tc->pos = d->pos;
        if (d->kind == DECL_INSTANCE) {
            tc_add_instance(tc, d, &d->body);
        } else if (d->kind == DECL_TYPE_INSTANCE) {
            type_instance(tc, d);
        }
    }
    for (i = 0; i < datas.len; i++) {
        const Decl *d = (const Decl *)data_decls.items[i];
        for (j = 0; j < d->deriving.len; j++) {
            const Deriving *dv = (const Deriving *)d->deriving.items[j];
            size_t k;
            tc->pos = d->pos;
            if (dv->via != NULL || dv->strategy == STRATEGY_NEWTYPE
                || dv->strategy == STRATEGY_ANYCLASS) {
                tc_fail(tc, xstrdup("this deriving strategy is not supported yet"));
            }
            for (k = 0; k < dv->classes.len; k++) {
                tc_derive(tc, d, (TcTyCon *)datas.items[i], (const Type *)dv->classes.items[k]);
            }
        }
    }
    /* Foreign imports. */
    for (i = 0; i < m->decls.len; i++) {
        const Decl *d = (const Decl *)m->decls.items[i];
        if (d->kind == DECL_FOREIGN) {
            TcVal *v = (TcVal *)xcalloc(1, sizeof *v);
            tc->pos = d->pos;
            v->type = conv_sigma(tc, d->ty);
            strmap_put(&tc->values, tkey(tc->module, d->name.name), v);
        } else if (d->kind == DECL_PAT_SYN || d->kind == DECL_PAT_SYN_SIG) {
            tc->pos = d->pos;
            tc_fail(tc, xstrdup("a pattern synonym is not supported yet"));
        }
    }
    tc_bind_group(tc, &m->decls, 1);
    for (i = 0; i < classes.len; i++) {
        check_defaults(tc, (TcClass *)classes.items[i]);
    }
    for (i = 0; i < tc->info->instances.len; i++) {
        check_instance(tc, (TcInst *)tc->info->instances.items[i]);
    }
    tc->path = path;
    solve_final(tc);
}

Tc *tc_new(const Program *p)
{
    Tc *tc = (Tc *)xcalloc(1, sizeof *tc);
    Vec none;
    tc->program = p;
    strmap_init(&tc->tycons);
    strmap_init(&tc->cons);
    strmap_init(&tc->classes);
    strmap_init(&tc->values);
    strmap_init(&tc->fixities);
    strmap_init(&tc->infos);
    strmap_init(&tc->errors);
    strmap_init(&tc->tuples);
    vec_init(&tc->wanted);
    vec_init(&tc->tyvars);
    strmap_init(&syn_infos);
    vec_init(&none);
    tc->tc_fun = (TcTyCon *)xcalloc(1, sizeof(TcTyCon));
    tc->tc_fun->kind = TC_PRIM;
    tc->tc_fun->module = "GHC.Prim";
    tc->tc_fun->name = "->";
    tc->tc_fun->arity = 2;
    tc->tc_unit = tc_tuple(tc, 0);
    return tc;
}

/* The imports of a module, with the implicit Prelude (char *). */
static Vec module_imports(const Module *m, const Vec *exts)
{
    Vec out;
    size_t i;
    vec_init(&out);
    for (i = 0; i < m->imports.len; i++) {
        vec_push(&out, ((const Import *)m->imports.items[i])->module);
    }
    if (implicit_prelude(exts, m)) {
        vec_push(&out, "Prelude");
    }
    return out;
}

static void check_in_order(Tc *tc, const Package *package, const SourceModule *sm,
                           const StrMap *by_name, Vec *active)
{
    const Module *m = sm->module;
    Vec imports;
    size_t i;
    TcError *err;
    if (strmap_has(&tc->errors, m->name)) {
        return;
    }
    if (vec_has_str(active, m->name)) {
        err = (TcError *)xcalloc(1, sizeof *err);
        err->message = xstrdup("typecheck: mutually recursive modules are not supported");
        strmap_put(&tc->errors, m->name, err);
        return;
    }
    vec_push(active, m->name);
    imports = module_imports(m, &package->extensions);
    for (i = 0; i < imports.len; i++) {
        const SourceModule *dep = (const SourceModule *)strmap_get(by_name, imports.items[i]);
        if (dep != NULL) {
            check_in_order(tc, package, dep, by_name, active);
        }
    }
    vec_pop(active);
    for (i = 0; i < imports.len; i++) {
        const char *name = (const char *)imports.items[i];
        Pos pos;
        if (strmap_has(&tc->errors, name) && tc_module_error(tc, name, &pos) != NULL) {
            const TcError *dep = (const TcError *)strmap_get(&tc->errors, name);
            err = (TcError *)xcalloc(1, sizeof *err);
            if (str_starts_with(dep->message, "typecheck: the imported module")) {
                err->message = dep->message;
            } else {
                err->message = str_printf("typecheck: the imported module %s does not "
                                          "typecheck: %lu:%lu: %s",
                                          name, dep->pos.line, dep->pos.col, dep->message);
            }
            strmap_put(&tc->errors, m->name, err);
            return;
        }
        if (!strmap_has(&tc->errors, name)) {
            err = (TcError *)xcalloc(1, sizeof *err);
            err->message = str_printf("typecheck: the imported module %s is not available",
                                      name);
            strmap_put(&tc->errors, m->name, err);
            return;
        }
    }
    if (setjmp(tc->jump) != 0) {
        err = (TcError *)xcalloc(1, sizeof *err);
        err->message = str_printf("typecheck: %s", tc->error);
        err->pos = tc->pos;
        strmap_put(&tc->errors, m->name, err);
        return;
    }
    check_module(tc, sm->path, m, &package->extensions);
    strmap_put(&tc->errors, m->name, NULL);
}

void tc_package(Tc *tc, const Package *package)
{
    StrMap by_name;
    Vec active;
    size_t i;
    strmap_init(&by_name);
    for (i = 0; i < package->modules.len; i++) {
        const SourceModule *sm = (const SourceModule *)package->modules.items[i];
        strmap_put(&by_name, sm->module->name, (void *)sm);
    }
    for (i = 0; i < package->modules.len; i++) {
        vec_init(&active);
        check_in_order(tc, package, (const SourceModule *)package->modules.items[i], &by_name,
                       &active);
    }
}

const char *tc_module_error(const Tc *tc, const char *module, Pos *pos)
{
    const TcError *e;
    pos->line = 0;
    pos->col = 0;
    if (!strmap_has(&tc->errors, module)) {
        return "typecheck: the module was not checked";
    }
    e = (const TcError *)strmap_get(&tc->errors, module);
    if (e == NULL) {
        return NULL;
    }
    *pos = e->pos;
    return e->message;
}

const TcModuleInfo *tc_info(const Tc *tc, const char *module)
{
    return (const TcModuleInfo *)strmap_get(&tc->infos, module);
}

int tc_ev_is_int(TcEv *e)
{
    Ty *h;
    e = tc_ev(e);
    if (e->kind != EV_INST || e->inst->head.len != 1) {
        return 0;
    }
    h = prune((Ty *)e->inst->head.items[0]);
    return h->kind == T_CON && str_eq(h->con->name, "Int") && str_eq(h->con->module, "GHC.Types");
}
