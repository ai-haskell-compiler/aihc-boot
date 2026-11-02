/* Expressions, patterns and binding groups. See tc_int.h.
 *
 * Checking is bidirectional: tc_expr checks an expression against an
 * expected type, which is a fresh meta variable when the type is not
 * known. A function application first finds the types of the
 * arguments from the type of the function, so a lambda argument sees
 * the types of its parameters, and an argument of a rank-n parameter
 * is checked against its forall type. `f $ x` is checked as `f x`.
 *
 * A binding group without signatures is split into strongly connected
 * components. Each component is inferred at a deeper level, then
 * generalized: the meta variables of the deeper level that are in the
 * types become bound variables, and the wanted constraints on them
 * become dictionary parameters. The monomorphism restriction keeps
 * pattern bindings and bindings without arguments from taking
 * dictionaries.
 */
#include <string.h>

#include "tc_int.h"

/* --- Local scopes ----------------------------------------------------------- */

void local_push(Tc *tc, const char *name, Ty *type, MonoGroup *mono)
{
    if (tc->nlocals == tc->cap_locals) {
        tc->cap_locals = tc->cap_locals ? tc->cap_locals * 2 : 64;
        tc->locals = (TcLocal *)xrealloc(tc->locals, tc->cap_locals * sizeof *tc->locals);
    }
    tc->locals[tc->nlocals].name = name;
    tc->locals[tc->nlocals].type = type;
    tc->locals[tc->nlocals].mono = mono;
    tc->nlocals++;
}

const TcLocal *local_lookup(const Tc *tc, const char *name)
{
    size_t i = tc->nlocals;
    while (i > 0) {
        i--;
        if (str_eq(tc->locals[i].name, name)) {
            return &tc->locals[i];
        }
    }
    return NULL;
}

static TcLocal *local_lookup_mut(Tc *tc, const char *name)
{
    return (TcLocal *)local_lookup(tc, name);
}

void tyvar_push(Tc *tc, const char *name, Ty *type)
{
    TcTyVar *v = (TcTyVar *)xmalloc(sizeof *v);
    v->name = name;
    v->type = type;
    vec_push(&tc->tyvars, v);
}

Ty *tyvar_lookup(const Tc *tc, const char *name)
{
    size_t i = tc->tyvars.len;
    while (i > 0) {
        const TcTyVar *v;
        i--;
        v = (const TcTyVar *)tc->tyvars.items[i];
        if (str_eq(v->name, name)) {
            return v->type;
        }
    }
    return NULL;
}

static TcNote *note_of(TcNote **slot)
{
    if (*slot == NULL) {
        *slot = tc_note_new();
    }
    return *slot;
}

static TcNote *qnote(const QName *q)
{
    return note_of(&((QName *)q)->note);
}

static TcNote *enote(const Expr *e)
{
    return note_of(&((Expr *)e)->note);
}

static TcNote *pnote(const Pat *p)
{
    return note_of(&((Pat *)p)->note);
}

static TcNote *dnote(const Decl *d)
{
    return note_of(&((Decl *)d)->note);
}

static void set_pos(Tc *tc, Span span)
{
    if (span.start.line != 0) {
        tc->pos = span.start;
    }
}

/* --- Known names ---------------------------------------------------------------- */

static TcClass *need_class(Tc *tc, const char *module, const char *name)
{
    TcClass *c = tc_known_class(tc, module, name);
    if (c == NULL) {
        tc_fail(tc, str_printf("the boot base does not define the class %s.%s, which the "
                               "typechecker needs",
                               module, name));
    }
    return c;
}

static Ty *need_type(Tc *tc, const char *module, const char *name)
{
    Ty *t = tc_known_type(tc, module, name);
    if (t == NULL) {
        tc_fail(tc, str_printf("the boot base does not define the type %s.%s, which the "
                               "typechecker needs",
                               module, name));
    }
    return t;
}

static TcEv *want1(Tc *tc, const char *module, const char *cls, Ty *t)
{
    Vec args;
    vec_init(&args);
    vec_push(&args, t);
    return emit_wanted(tc, pred_new(need_class(tc, module, cls), &args));
}

static Ty *char_type(Tc *tc)
{
    return need_type(tc, "GHC.Types", "Char");
}

static Ty *bool_type(Tc *tc)
{
    return need_type(tc, "GHC.Types", "Bool");
}

/* Split a function type, making it one when it is a meta variable. */
static void split_fun(Tc *tc, Ty *t, Ty **a, Ty **b)
{
    Ty *pt = ty_whnf(tc, t);
    if (ty_is_fun(tc, pt, a, b)) {
        return;
    }
    if (pt->kind == T_FORALL) {
        pt = instantiate(tc, pt, NULL);
        if (ty_is_fun(tc, pt, a, b)) {
            return;
        }
    }
    *a = ty_meta(tc);
    *b = ty_meta(tc);
    unify(tc, pt, ty_fun(tc, *a, *b));
}

Fixity tc_fixity(Tc *tc, const QName *op)
{
    const char *name = bare(op->name);
    const Entity *e;
    const Fixity *f;
    if (op->qual[0] == '\0' && local_lookup(tc, name) != NULL) {
        return fixity_default();
    }
    e = tc_entity(tc, op, SPACE_TERM);
    if (e == NULL) {
        return fixity_default();
    }
    f = (const Fixity *)strmap_get(&tc->fixities, tkey(e->module, e->name));
    return f != NULL ? *f : fixity_default();
}

static int is_dollar(Tc *tc, const QName *op)
{
    const Entity *e;
    if (op->qual[0] == '\0' && local_lookup(tc, bare(op->name)) != NULL) {
        return 0;
    }
    e = tc_entity(tc, op, SPACE_TERM);
    return e != NULL && str_eq(e->module, "GHC.Base") && str_eq(e->name, "$");
}

/* --- Occurrences ----------------------------------------------------------------- */

/* The uninstantiated type of a name, and its group when it is under
 * inference. */
static Ty *name_sigma(Tc *tc, const QName *q, MonoGroup **mono)
{
    const char *name = bare(q->name);
    const Entity *e;
    TcCon *con;
    TcVal *v;
    *mono = NULL;
    set_pos(tc, q->span);
    if (q->qual[0] == '\0') {
        const TcLocal *l = local_lookup(tc, name);
        if (l != NULL) {
            *mono = l->mono;
            return l->type;
        }
    }
    con = tc_con(tc, q);
    if (con != NULL) {
        return con->scheme;
    }
    e = tc_entity(tc, q, SPACE_TERM);
    if (e == NULL) {
        tc_fail(tc, str_printf("unbound name %s%s%s", q->qual[0] == TC_DIRECT ? "" : q->qual,
                               q->qual[0] != '\0' && q->qual[0] != TC_DIRECT ? "." : "",
                               name));
    }
    v = tc_value(tc, e->module, e->name);
    if (v == NULL) {
        tc_fail(tc, str_printf("the name %s.%s has no type", e->module, e->name));
    }
    *mono = v->mono;
    return v->type;
}

/* The instantiated type of an occurrence of a name. */
static Ty *name_type(Tc *tc, const QName *q)
{
    MonoGroup *mono;
    Ty *sigma = name_sigma(tc, q, &mono);
    if (mono != NULL) {
        vec_push(&mono->occs, qnote(q));
        return sigma;
    }
    if (prune(sigma)->kind != T_FORALL) {
        return sigma;
    }
    return instantiate(tc, sigma, &qnote(q)->evs);
}

/* --- Applications ---------------------------------------------------------------- */

/* An argument: an expression or a node of an operator chain. */
typedef struct {
    const Expr *e;
    const FNode *n;
} Arg;

static void tc_fnode(Tc *tc, const FNode *n, Ty *expected);

static void tc_arg(Tc *tc, Arg a, Ty *ty)
{
    if (a.e != NULL) {
        tc_expr_sigma(tc, a.e, ty);
        return;
    }
    if (a.n->kind == FN_LEAF) {
        tc_expr_sigma(tc, (const Expr *)a.n->leaf, ty);
        return;
    }
    if (prune(ty)->kind == T_FORALL) {
        /* Check against the skolemized type. A context would need a
         * dictionary lambda, which an operator node has no note for. */
        Ty *t = prune(ty);
        size_t ntyvars = tc->tyvars.len;
        tc->level++;
        while (t->kind == T_FORALL) {
            Vec sk;
            size_t i;
            if (t->preds.len > 0) {
                tc_fail(tc, xstrdup("an operator expression as an argument of a constrained "
                                    "polymorphic type is not supported yet"));
            }
            vec_init(&sk);
            for (i = 0; i < t->vars.len; i++) {
                vec_push(&sk, ty_skol(tc, ((Ty *)t->vars.items[i])->name));
            }
            t = prune(ty_subst(t->body, &t->vars, &sk));
        }
        tc_fnode(tc, a.n, t);
        solve(tc);
        tc->tyvars.len = ntyvars;
        tc->level--;
        return;
    }
    tc_fnode(tc, a.n, ty);
}

/* Apply a function of type fty to args, with the result of type
 * expected. */
static void tc_spine(Tc *tc, Ty *fty, Arg *args, size_t n, Ty *expected)
{
    Ty **argtys = (Ty **)xmalloc((n + 1) * sizeof *argtys);
    size_t i;
    Ty *res;
    for (i = 0; i < n; i++) {
        Ty *a;
        Ty *b;
        Ty *p = prune(fty);
        if (p->kind == T_FORALL) {
            Vec evs;
            vec_init(&evs);
            p = instantiate(tc, p, &evs);
            if (evs.len > 0) {
                tc_fail(tc, xstrdup("a constrained rank-n result in an application is not "
                                    "supported yet"));
            }
        }
        split_fun(tc, p, &a, &b);
        argtys[i] = a;
        fty = b;
    }
    res = prune(fty);
    if (res->kind == T_FORALL) {
        Vec evs;
        vec_init(&evs);
        res = instantiate(tc, res, &evs);
        if (evs.len > 0) {
            tc_fail(tc, xstrdup("a constrained rank-n result in an application is not "
                                "supported yet"));
        }
    }
    unify(tc, res, expected);
    for (i = 0; i < n; i++) {
        tc_arg(tc, args[i], argtys[i]);
    }
}

/* The type of the head of an application. */
static Ty *tc_head(Tc *tc, const Expr *e)
{
    Ty *t;
    switch (e->kind) {
    case EXPR_VAR:
    case EXPR_CON:
        return name_type(tc, &e->name);
    case EXPR_PAREN:
        return tc_head(tc, e->a);
    case EXPR_TYPE_APP: {
        /* `f @t1 @t2`: instantiate the first variables of f's type. */
        Vec types;
        const Expr *h = e;
        MonoGroup *mono;
        Ty *sigma;
        Ty *p;
        size_t i;
        vec_init(&types);
        while (h->kind == EXPR_TYPE_APP) {
            vec_insert(&types, 0, conv_open(tc, h->ty));
            h = h->a;
            while (h->kind == EXPR_PAREN) {
                h = h->a;
            }
        }
        if (h->kind != EXPR_VAR && h->kind != EXPR_CON) {
            tc_fail(tc, xstrdup("a type application to this expression is not supported yet"));
        }
        sigma = name_sigma(tc, &h->name, &mono);
        p = prune(sigma);
        if (mono != NULL || p->kind != T_FORALL || p->vars.len < types.len) {
            tc_fail(tc, xstrdup("a type application to a name without enough type variables"));
        }
        for (i = types.len; i < p->vars.len; i++) {
            vec_push(&types, ty_meta(tc));
        }
        t = instantiate_with(tc, p, &types, &qnote(&h->name)->evs);
        return instantiate(tc, t, &qnote(&h->name)->evs);
    }
    default:
        t = ty_meta(tc);
        tc_expr(tc, e, t);
        return t;
    }
}

static void tc_app(Tc *tc, const Expr *e, Ty *expected)
{
    Vec args;
    const Expr *head = e;
    Arg *as;
    size_t i;
    vec_init(&args);
    while (head->kind == EXPR_APP) {
        vec_insert(&args, 0, head->b);
        head = head->a;
        while (head->kind == EXPR_PAREN && head->a->kind == EXPR_APP) {
            head = head->a;
        }
    }
    as = (Arg *)xmalloc((args.len + 1) * sizeof *as);
    for (i = 0; i < args.len; i++) {
        as[i].e = (const Expr *)args.items[i];
        as[i].n = NULL;
    }
    tc_spine(tc, tc_head(tc, head), as, args.len, expected);
}

/* --- Operator chains --------------------------------------------------------------- */

static FNode *resolve_chain(Tc *tc, const Expr *x)
{
    Vec toks;
    size_t i;
    char *err = NULL;
    FNode *tree;
    const Expr *first = x->a;
    vec_init(&toks);
    if (first->kind == EXPR_NEG) {
        ftok_push(&toks, FT_NEG, first, NULL, fixity_default());
        first = first->a;
    }
    ftok_push(&toks, FT_EXP, first, NULL, fixity_default());
    for (i = 0; i < x->steps.len; i++) {
        const InfixStep *step = (const InfixStep *)x->steps.items[i];
        const Expr *operand = (const Expr *)step->operand;
        ftok_push(&toks, FT_OP, NULL, &step->op, tc_fixity(tc, &step->op));
        if (operand->kind == EXPR_NEG) {
            ftok_push(&toks, FT_NEG, operand, NULL, fixity_default());
            operand = operand->a;
        }
        ftok_push(&toks, FT_EXP, operand, NULL, fixity_default());
    }
    tree = fixity_resolve(&toks, &err);
    if (tree == NULL) {
        tc_fail(tc, err);
    }
    return tree;
}

static void tc_fnode(Tc *tc, const FNode *n, Ty *expected)
{
    Arg args[2];
    switch (n->kind) {
    case FN_LEAF:
        tc_expr(tc, (const Expr *)n->leaf, expected);
        return;
    case FN_NEG:
        vec_push(&enote((const Expr *)n->leaf)->evs, want1(tc, "GHC.Num", "Num", expected));
        tc_fnode(tc, n->l, expected);
        return;
    default:
        set_pos(tc, n->op->span);
        if (is_dollar(tc, n->op)) {
            /* `f $ x` is checked as `f x`, so x may be polymorphic. */
            Ty *fty;
            if (n->l->kind == FN_LEAF) {
                fty = tc_head(tc, (const Expr *)n->l->leaf);
            } else {
                fty = ty_meta(tc);
                tc_fnode(tc, n->l, fty);
            }
            args[0].e = NULL;
            args[0].n = n->r;
            tc_spine(tc, fty, args, 1, expected);
            return;
        }
        args[0].e = NULL;
        args[0].n = n->l;
        args[1].e = NULL;
        args[1].n = n->r;
        tc_spine(tc, name_type(tc, n->op), args, 2, expected);
        return;
    }
}

/* --- Patterns ------------------------------------------------------------------------ */

/* The givens that the patterns of one match bring into scope. */
typedef struct {
    Vec preds;
    Vec evs;
} PatGivens;

static void pg_init(PatGivens *g)
{
    vec_init(&g->preds);
    vec_init(&g->evs);
}

static void tc_pat(Tc *tc, const Pat *p, Ty *ty, PatGivens *g);

static FNode *resolve_pat_chain(Tc *tc, const Pat *p)
{
    Vec toks;
    size_t i;
    char *err = NULL;
    FNode *tree;
    vec_init(&toks);
    ftok_push(&toks, FT_EXP, p->a, NULL, fixity_default());
    for (i = 0; i < p->steps.len; i++) {
        const InfixStep *step = (const InfixStep *)p->steps.items[i];
        ftok_push(&toks, FT_OP, NULL, &step->op, tc_fixity(tc, &step->op));
        ftok_push(&toks, FT_EXP, step->operand, NULL, fixity_default());
    }
    tree = fixity_resolve(&toks, &err);
    if (tree == NULL) {
        tc_fail(tc, err);
    }
    return tree;
}

/* A constructor applied to argument patterns. */
static void tc_con_pat(Tc *tc, const Pat *p, const QName *cname, const Pat **args, size_t n,
                       Ty *ty, PatGivens *g)
{
    TcCon *con = tc_con(tc, cname);
    Vec univ;
    Vec all_from;
    Vec all_to;
    size_t i;
    Vec targs;
    set_pos(tc, cname->span);
    if (con == NULL) {
        tc_fail(tc, str_printf("unbound constructor %s", cname->name));
    }
    if (con->fields.len != n) {
        tc_fail(tc, str_printf("the constructor %s takes %lu arguments, but the pattern has %lu",
                               con->name, (unsigned long)con->fields.len, (unsigned long)n));
    }
    vec_init(&univ);
    vec_init(&all_from);
    vec_init(&all_to);
    for (i = 0; i < con->univ.len; i++) {
        Ty *m = ty_meta(tc);
        vec_push(&univ, m);
        vec_push(&all_from, con->univ.items[i]);
        vec_push(&all_to, m);
    }
    for (i = 0; i < con->exist.len; i++) {
        vec_push(&all_from, con->exist.items[i]);
        vec_push(&all_to, ty_skol(tc, ((Ty *)con->exist.items[i])->name));
    }
    vec_init(&targs);
    unify(tc, ty, ty_apps(ty_con(con->tycon), &univ));
    for (i = 0; i < con->ctx.len; i++) {
        TcPred *cp = pred_subst((const TcPred *)con->ctx.items[i], &all_from, &all_to);
        TcEv *ev = ev_new(EV_VAR);
        if (cp->cls != NULL) {
            ev->var = fresh_dict(tc);
            if (p != NULL) {
                vec_push(&pnote(p)->dicts, (void *)ev->var);
            }
        }
        vec_push(&g->preds, cp);
        vec_push(&g->evs, ev);
    }
    if (con->ctx.len > 0 && p == NULL) {
        tc_fail(tc, xstrdup("an existential constructor in an operator pattern is not "
                            "supported yet"));
    }
    for (i = 0; i < n; i++) {
        tc_pat(tc, args[i], ty_subst((Ty *)con->fields.items[i], &all_from, &all_to), g);
    }
}

static void tc_fnode_pat(Tc *tc, const FNode *n, Ty *ty, PatGivens *g)
{
    if (n->kind == FN_LEAF) {
        tc_pat(tc, (const Pat *)n->leaf, ty, g);
        return;
    }
    if (n->kind != FN_BIN) {
        tc_fail(tc, xstrdup("negation in a pattern chain"));
    }
    {
        TcCon *con = tc_con(tc, n->op);
        Ty *lt;
        Ty *rt;
        Vec univ;
        size_t i;
        if (con == NULL) {
            tc_fail(tc, str_printf("unbound constructor %s", n->op->name));
        }
        if (con->fields.len != 2 || con->exist.len > 0 || con->ctx.len > 0) {
            tc_fail(tc, str_printf("the constructor %s in an operator pattern must have two "
                                   "fields",
                                   con->name));
        }
        vec_init(&univ);
        for (i = 0; i < con->univ.len; i++) {
            vec_push(&univ, ty_meta(tc));
        }
        unify(tc, ty, ty_apps(ty_con(con->tycon), &univ));
        lt = ty_subst((Ty *)con->fields.items[0], &con->univ, &univ);
        rt = ty_subst((Ty *)con->fields.items[1], &con->univ, &univ);
        tc_fnode_pat(tc, n->l, lt, g);
        tc_fnode_pat(tc, n->r, rt, g);
    }
}

static int record_field_index(const TcCon *con, const char *field)
{
    size_t i;
    for (i = 0; i < con->field_names.len; i++) {
        const char *f = (const char *)con->field_names.items[i];
        if (f != NULL && str_eq(f, field)) {
            return (int)i;
        }
    }
    return -1;
}

static void tc_pat(Tc *tc, const Pat *p, Ty *ty, PatGivens *g)
{
    size_t i;
    switch (p->kind) {
    case PAT_VAR:
        set_pos(tc, p->var.span);
        local_push(tc, p->var.name, ty, NULL);
        return;
    case PAT_WILDCARD:
    case PAT_TYPE_ARG:
        return;
    case PAT_LIT:
        set_pos(tc, p->span);
        switch (p->lit.kind) {
        case LIT_INTEGER:
            vec_push(&pnote(p)->evs, want1(tc, "GHC.Classes", "Eq", ty));
            vec_push(&pnote(p)->evs, want1(tc, "GHC.Num", "Num", ty));
            return;
        case LIT_FLOAT:
            vec_push(&pnote(p)->evs, want1(tc, "GHC.Classes", "Eq", ty));
            vec_push(&pnote(p)->evs, want1(tc, "GHC.Real", "Fractional", ty));
            return;
        case LIT_CHAR:
            unify(tc, ty, char_type(tc));
            return;
        case LIT_STRING:
            if (tc->overloaded_strings) {
                vec_push(&pnote(p)->evs, want1(tc, "GHC.Classes", "Eq", ty));
                vec_push(&pnote(p)->evs, want1(tc, "Data.String", "IsString", ty));
                return;
            }
            unify(tc, ty, ty_list(tc, char_type(tc)));
            return;
        }
        return;
    case PAT_CON: {
        const Pat **args = (const Pat **)xmalloc((p->args.len + 1) * sizeof *args);
        size_t n = 0;
        for (i = 0; i < p->args.len; i++) {
            const Pat *a = (const Pat *)p->args.items[i];
            if (a->kind != PAT_TYPE_ARG) {
                args[n++] = a;
            }
        }
        if (p->con.qual[0] == '\0' && str_eq(p->con.name, "[]") && n == 0) {
            unify(tc, ty, ty_list(tc, ty_meta(tc)));
            return;
        }
        if (p->con.qual[0] == '\0'
            && (str_eq(p->con.name, "()") || str_starts_with(p->con.name, "(,"))) {
            size_t arity = str_eq(p->con.name, "()") ? 0 : strlen(p->con.name) - 1;
            Vec items;
            vec_init(&items);
            for (i = 0; i < arity; i++) {
                vec_push(&items, ty_meta(tc));
            }
            unify(tc, ty, ty_apps(ty_con(tc_tuple(tc, arity)), &items));
            if (n != arity) {
                tc_fail(tc, xstrdup("a tuple constructor pattern with the wrong number of "
                                    "arguments"));
            }
            for (i = 0; i < n; i++) {
                tc_pat(tc, args[i], (Ty *)items.items[i], g);
            }
            return;
        }
        tc_con_pat(tc, p, &p->con, args, n, ty, g);
        return;
    }
    case PAT_INFIX:
        tc_fnode_pat(tc, resolve_pat_chain(tc, p), ty, g);
        return;
    case PAT_RECORD: {
        TcCon *con = tc_con(tc, &p->con);
        Vec univ;
        Vec seen;
        set_pos(tc, p->con.span);
        if (con == NULL) {
            tc_fail(tc, str_printf("unbound constructor %s", p->con.name));
        }
        if (con->exist.len > 0 || con->ctx.len > 0) {
            tc_fail(tc, xstrdup("a record pattern of an existential constructor is not "
                                "supported yet"));
        }
        vec_init(&univ);
        vec_init(&seen);
        for (i = 0; i < con->univ.len; i++) {
            vec_push(&univ, ty_meta(tc));
        }
        unify(tc, ty, ty_apps(ty_con(con->tycon), &univ));
        for (i = 0; i < p->fields.len; i++) {
            const FieldPat *f = (const FieldPat *)p->fields.items[i];
            const char *fname = bare(f->name.name);
            int k = record_field_index(con, fname);
            Ty *ft;
            if (k < 0) {
                tc_fail(tc, str_printf("the constructor %s has no field %s", con->name, fname));
            }
            vec_push(&seen, (void *)fname);
            ft = ty_subst((Ty *)con->fields.items[k], &con->univ, &univ);
            if (f->pat != NULL) {
                tc_pat(tc, f->pat, ft, g);
            } else {
                local_push(tc, fname, ft, NULL);
            }
        }
        if (p->wildcard) {
            for (i = 0; i < con->field_names.len; i++) {
                const char *fname = (const char *)con->field_names.items[i];
                if (fname != NULL && !vec_has_str(&seen, fname)) {
                    local_push(tc, fname, ty_subst((Ty *)con->fields.items[i], &con->univ, &univ),
                               NULL);
                }
            }
        }
        return;
    }
    case PAT_TUPLE: {
        Vec items;
        vec_init(&items);
        for (i = 0; i < p->args.len; i++) {
            vec_push(&items, ty_meta(tc));
        }
        unify(tc, ty, ty_apps(ty_con(tc_tuple(tc, p->args.len)), &items));
        for (i = 0; i < p->args.len; i++) {
            tc_pat(tc, (const Pat *)p->args.items[i], (Ty *)items.items[i], g);
        }
        return;
    }
    case PAT_LIST: {
        Ty *elem = ty_meta(tc);
        unify(tc, ty, ty_list(tc, elem));
        for (i = 0; i < p->args.len; i++) {
            tc_pat(tc, (const Pat *)p->args.items[i], elem, g);
        }
        return;
    }
    case PAT_PAREN:
    case PAT_BANG:
    case PAT_LAZY:
        tc_pat(tc, p->a, ty, g);
        return;
    case PAT_AS:
        set_pos(tc, p->var.span);
        local_push(tc, p->var.name, ty, NULL);
        tc_pat(tc, p->a, ty, g);
        return;
    case PAT_VIEW: {
        Ty *res = ty_meta(tc);
        tc_expr(tc, p->expr, ty_fun(tc, ty, res));
        tc_pat(tc, p->a, res, g);
        return;
    }
    case PAT_SIG:
        unify(tc, ty, conv_open(tc, p->ty));
        tc_pat(tc, p->a, ty, g);
        return;
    }
}

/* Check the pattern of a statement. The givens of an existential
 * constructor stay in scope for the rest of the statements: the caller
 * restores tc->givens at the end of the block. */
static void tc_pat_stmt(Tc *tc, const Pat *p, Ty *ty)
{
    PatGivens g;
    pg_init(&g);
    tc_pat(tc, p, ty, &g);
    if (g.preds.len > 0) {
        push_given_evs(tc, &g.preds, &g.evs);
    }
}

/* --- Right-hand sides --------------------------------------------------------------- */

static void tc_rhs(Tc *tc, const Rhs *rhs, Ty *ty);

static void tc_guards(Tc *tc, const GuardedRhs *g, Ty *ty)
{
    size_t mark = tc->nlocals;
    Givens *givens = tc->givens;
    size_t i;
    for (i = 0; i < g->guards.len; i++) {
        const Stmt *s = (const Stmt *)g->guards.items[i];
        switch (s->kind) {
        case STMT_EXPR:
            tc_expr(tc, s->expr, bool_type(tc));
            break;
        case STMT_BIND: {
            Ty *t = ty_meta(tc);
            tc_expr(tc, s->expr, t);
            tc_pat_stmt(tc, s->pat, t);
            break;
        }
        case STMT_LET:
            tc_bind_group(tc, &s->decls, 0);
            break;
        }
    }
    tc_expr(tc, g->body, ty);
    tc->nlocals = mark;
    tc->givens = givens;
}

static void tc_rhs(Tc *tc, const Rhs *rhs, Ty *ty)
{
    size_t mark = tc->nlocals;
    size_t i;
    tc_bind_group(tc, &rhs->wheres, 0);
    if (!rhs->guarded) {
        tc_expr(tc, rhs->plain, ty);
    } else {
        for (i = 0; i < rhs->guards.len; i++) {
            tc_guards(tc, (const GuardedRhs *)rhs->guards.items[i], ty);
        }
    }
    tc->nlocals = mark;
}

static void tc_alts(Tc *tc, const Vec *alts, Ty *scrut, Ty *res)
{
    size_t i;
    for (i = 0; i < alts->len; i++) {
        const Alt *alt = (const Alt *)alts->items[i];
        size_t mark = tc->nlocals;
        PatGivens g;
        tc->pos = alt->pos;
        pg_init(&g);
        tc_pat(tc, alt->pat, scrut, &g);
        if (g.preds.len > 0) {
            push_given_evs(tc, &g.preds, &g.evs);
        }
        tc_rhs(tc, alt->rhs, res);
        if (g.preds.len > 0) {
            pop_givens(tc);
        }
        tc->nlocals = mark;
    }
}

/* --- Statements -------------------------------------------------------------------- */

static void tc_do(Tc *tc, const Vec *stmts, Ty *m, Ty *expected)
{
    size_t mark = tc->nlocals;
    Givens *givens = tc->givens;
    size_t i;
    for (i = 0; i < stmts->len; i++) {
        const Stmt *s = (const Stmt *)stmts->items[i];
        int last = i + 1 == stmts->len;
        switch (s->kind) {
        case STMT_EXPR:
            if (last) {
                Ty *a = ty_meta(tc);
                unify(tc, expected, ty_app(m, a));
                tc_expr(tc, s->expr, expected);
            } else {
                tc_expr(tc, s->expr, ty_app(m, ty_meta(tc)));
            }
            break;
        case STMT_BIND: {
            Ty *t = ty_meta(tc);
            if (last) {
                tc_fail(tc, xstrdup("the last statement of a do block must be an expression"));
            }
            tc_expr(tc, s->expr, ty_app(m, t));
            tc_pat_stmt(tc, s->pat, t);
            break;
        }
        case STMT_LET:
            if (last) {
                tc_fail(tc, xstrdup("the last statement of a do block must be an expression"));
            }
            tc_bind_group(tc, &s->decls, 0);
            break;
        }
    }
    tc->nlocals = mark;
    tc->givens = givens;
}

static void tc_comp(Tc *tc, const Expr *body, const Vec *stmts, Ty *elem)
{
    size_t mark = tc->nlocals;
    Givens *givens = tc->givens;
    size_t i;
    for (i = 0; i < stmts->len; i++) {
        const Stmt *s = (const Stmt *)stmts->items[i];
        switch (s->kind) {
        case STMT_EXPR:
            tc_expr(tc, s->expr, bool_type(tc));
            break;
        case STMT_BIND: {
            Ty *t = ty_meta(tc);
            tc_expr(tc, s->expr, ty_list(tc, t));
            tc_pat_stmt(tc, s->pat, t);
            break;
        }
        case STMT_LET:
            tc_bind_group(tc, &s->decls, 0);
            break;
        }
    }
    tc_expr(tc, body, elem);
    tc->nlocals = mark;
    tc->givens = givens;
}

/* --- Records ---------------------------------------------------------------------- */

/* The constructor of a type that has a field, by the field's name. */
static TcCon *con_with_field(Tc *tc, const QName *field, int *index)
{
    const Entity *e = tc_entity(tc, field, SPACE_TERM);
    size_t i;
    StrMap *cons = &tc->cons;
    if (e == NULL) {
        tc_fail(tc, str_printf("unbound field %s", field->name));
    }
    for (i = 0; i < cons->len; i++) {
        TcCon *c = (TcCon *)cons->vals[i];
        int k;
        if (!str_eq(c->module, e->module)) {
            continue;
        }
        k = record_field_index(c, e->name);
        if (k >= 0) {
            *index = k;
            return c;
        }
    }
    tc_fail(tc, str_printf("no constructor has the field %s", field->name));
    return NULL;
}

static void tc_record_con(Tc *tc, const Expr *e, Ty *expected)
{
    TcCon *con = tc_con(tc, &e->name);
    Vec univ;
    size_t i;
    set_pos(tc, e->name.span);
    if (con == NULL) {
        tc_fail(tc, str_printf("unbound constructor %s", e->name.name));
    }
    if (con->exist.len > 0 || con->ctx.len > 0) {
        tc_fail(tc, xstrdup("record construction of an existential constructor is not "
                            "supported yet"));
    }
    if (e->wildcard) {
        tc_fail(tc, xstrdup("a record wildcard in an expression is not supported yet"));
    }
    vec_init(&univ);
    for (i = 0; i < con->univ.len; i++) {
        vec_push(&univ, ty_meta(tc));
    }
    unify(tc, expected, ty_apps(ty_con(con->tycon), &univ));
    for (i = 0; i < e->fields.len; i++) {
        const FieldUpdate *f = (const FieldUpdate *)e->fields.items[i];
        const char *fname = bare(f->name.name);
        int k = record_field_index(con, fname);
        Ty *ft;
        if (k < 0) {
            tc_fail(tc, str_printf("the constructor %s has no field %s", con->name, fname));
        }
        ft = ty_subst((Ty *)con->fields.items[k], &con->univ, &univ);
        if (f->value != NULL) {
            tc_expr_sigma(tc, f->value, ft);
        } else {
            QName q = f->name;
            Ty *vt;
            q.qual = "";
            vt = name_type(tc, &q);
            unify(tc, vt, ft);
            ((FieldUpdate *)f)->name.note = q.note;
        }
    }
}

static void tc_record_update(Tc *tc, const Expr *e, Ty *expected)
{
    const FieldUpdate *first;
    TcCon *con;
    int k;
    Vec univ;
    size_t i;
    if (e->fields.len == 0) {
        tc_fail(tc, xstrdup("an empty record update"));
    }
    first = (const FieldUpdate *)e->fields.items[0];
    set_pos(tc, first->name.span);
    con = con_with_field(tc, &first->name, &k);
    vec_init(&univ);
    for (i = 0; i < con->univ.len; i++) {
        vec_push(&univ, ty_meta(tc));
    }
    unify(tc, expected, ty_apps(ty_con(con->tycon), &univ));
    tc_expr(tc, e->a, expected);
    for (i = 0; i < e->fields.len; i++) {
        const FieldUpdate *f = (const FieldUpdate *)e->fields.items[i];
        TcCon *c = con_with_field(tc, &f->name, &k);
        Ty *ft = ty_subst((Ty *)c->fields.items[k], &c->univ, &univ);
        if (c->tycon != con->tycon) {
            tc_fail(tc, xstrdup("a record update with fields of different types"));
        }
        if (f->value == NULL) {
            tc_fail(tc, xstrdup("a field pun in a record update is not supported yet"));
        }
        tc_expr_sigma(tc, f->value, ft);
    }
}

/* --- Expressions ------------------------------------------------------------------- */

void tc_expr_sigma(Tc *tc, const Expr *e, Ty *sigma)
{
    Ty *t = prune(sigma);
    Vec dicts;
    Vec skolems;
    size_t ntyvars = tc->tyvars.len;
    int pushed = 0;
    if (t->kind != T_FORALL) {
        tc_expr(tc, e, t);
        return;
    }
    vec_init(&dicts);
    vec_init(&skolems);
    tc->level++;
    while (t->kind == T_FORALL) {
        Vec preds;
        size_t i;
        size_t start = skolems.len;
        Vec sk;
        vec_init(&sk);
        for (i = 0; i < t->vars.len; i++) {
            Ty *s = ty_skol(tc, ((Ty *)t->vars.items[i])->name);
            vec_push(&sk, s);
            vec_push(&skolems, s);
            if (t->scoped) {
                tyvar_push(tc, s->name, s);
            }
        }
        (void)start;
        vec_init(&preds);
        for (i = 0; i < t->preds.len; i++) {
            vec_push(&preds, pred_subst((const TcPred *)t->preds.items[i], &t->vars, &sk));
        }
        if (preds.len > 0) {
            push_givens(tc, &preds, &dicts);
            pushed++;
        }
        t = prune(ty_subst(t->body, &t->vars, &sk));
    }
    tc_expr(tc, e, t);
    solve(tc);
    while (pushed-- > 0) {
        pop_givens(tc);
    }
    tc->tyvars.len = ntyvars;
    tc->level--;
    if (dicts.len > 0) {
        TcNote *n = enote(e);
        size_t i;
        for (i = 0; i < dicts.len; i++) {
            vec_push(&n->lams, dicts.items[i]);
        }
    }
}

static Ty *tuple_type(Tc *tc, size_t n, Vec *items)
{
    size_t i;
    vec_init(items);
    for (i = 0; i < n; i++) {
        vec_push(items, ty_meta(tc));
    }
    return ty_apps(ty_con(tc_tuple(tc, n)), items);
}

void tc_expr(Tc *tc, const Expr *e, Ty *expected)
{
    size_t i;
    switch (e->kind) {
    case EXPR_VAR:
    case EXPR_CON:
        unify(tc, name_type(tc, &e->name), expected);
        return;
    case EXPR_LIT:
        set_pos(tc, e->span);
        switch (e->lit.kind) {
        case LIT_INTEGER:
            vec_push(&enote(e)->evs, want1(tc, "GHC.Num", "Num", expected));
            return;
        case LIT_FLOAT:
            vec_push(&enote(e)->evs, want1(tc, "GHC.Real", "Fractional", expected));
            return;
        case LIT_CHAR:
            unify(tc, expected, char_type(tc));
            return;
        case LIT_STRING:
            if (tc->overloaded_strings) {
                vec_push(&enote(e)->evs, want1(tc, "Data.String", "IsString", expected));
                return;
            }
            unify(tc, expected, ty_list(tc, char_type(tc)));
            return;
        }
        return;
    case EXPR_APP:
        tc_app(tc, e, expected);
        return;
    case EXPR_TYPE_APP:
        unify(tc, tc_head(tc, e), expected);
        return;
    case EXPR_PAREN:
        tc_expr(tc, e->a, expected);
        return;
    case EXPR_INFIX:
        tc_fnode(tc, resolve_chain(tc, e), expected);
        return;
    case EXPR_NEG:
        vec_push(&enote(e)->evs, want1(tc, "GHC.Num", "Num", expected));
        tc_expr(tc, e->a, expected);
        return;
    case EXPR_LAMBDA: {
        size_t mark = tc->nlocals;
        PatGivens g;
        Ty *t = expected;
        pg_init(&g);
        for (i = 0; i < e->pats.len; i++) {
            Ty *a;
            Ty *b;
            split_fun(tc, t, &a, &b);
            tc_pat(tc, (const Pat *)e->pats.items[i], a, &g);
            t = b;
        }
        if (g.preds.len > 0) {
            push_given_evs(tc, &g.preds, &g.evs);
        }
        tc_expr(tc, e->a, t);
        if (g.preds.len > 0) {
            pop_givens(tc);
        }
        tc->nlocals = mark;
        return;
    }
    case EXPR_LAMBDA_CASE: {
        Ty *a;
        Ty *b;
        split_fun(tc, expected, &a, &b);
        tc_alts(tc, &e->alts, a, b);
        return;
    }
    case EXPR_LET: {
        size_t mark = tc->nlocals;
        tc_bind_group(tc, &e->decls, 0);
        tc_expr(tc, e->a, expected);
        tc->nlocals = mark;
        return;
    }
    case EXPR_IF:
        tc_expr(tc, e->a, bool_type(tc));
        tc_expr(tc, e->b, expected);
        tc_expr(tc, e->c, expected);
        return;
    case EXPR_MULTI_IF:
        for (i = 0; i < e->guards.len; i++) {
            tc_guards(tc, (const GuardedRhs *)e->guards.items[i], expected);
        }
        return;
    case EXPR_CASE: {
        Ty *t = ty_meta(tc);
        tc_expr(tc, e->a, t);
        tc_alts(tc, &e->alts, t, expected);
        return;
    }
    case EXPR_DO: {
        Ty *m = ty_meta(tc);
        if (e->stmts.len == 0) {
            tc_fail(tc, xstrdup("an empty do block"));
        }
        vec_push(&enote(e)->evs, want1(tc, "GHC.Base", "Monad", m));
        tc_do(tc, &e->stmts, m, expected);
        return;
    }
    case EXPR_TUPLE: {
        Vec items;
        unify(tc, expected, tuple_type(tc, e->items.len, &items));
        for (i = 0; i < e->items.len; i++) {
            tc_expr(tc, (const Expr *)e->items.items[i], (Ty *)items.items[i]);
        }
        return;
    }
    case EXPR_TUPLE_SECTION: {
        Vec items;
        Ty *t = tuple_type(tc, e->items.len, &items);
        i = e->items.len;
        while (i > 0) {
            i--;
            if (e->items.items[i] == NULL) {
                t = ty_fun(tc, (Ty *)items.items[i], t);
            }
        }
        unify(tc, expected, t);
        for (i = 0; i < e->items.len; i++) {
            if (e->items.items[i] != NULL) {
                tc_expr(tc, (const Expr *)e->items.items[i], (Ty *)items.items[i]);
            }
        }
        return;
    }
    case EXPR_LIST: {
        Ty *elem = ty_meta(tc);
        set_pos(tc, e->span);
        unify(tc, expected, ty_list(tc, elem));
        for (i = 0; i < e->items.len; i++) {
            tc_expr(tc, (const Expr *)e->items.items[i], elem);
        }
        return;
    }
    case EXPR_ARITH_SEQ: {
        Ty *elem = ty_meta(tc);
        unify(tc, expected, ty_list(tc, elem));
        vec_push(&enote(e)->evs, want1(tc, "GHC.Enum", "Enum", elem));
        tc_expr(tc, e->a, elem);
        if (e->b != NULL) {
            tc_expr(tc, e->b, elem);
        }
        if (e->c != NULL) {
            tc_expr(tc, e->c, elem);
        }
        return;
    }
    case EXPR_LIST_COMP: {
        Ty *elem = ty_meta(tc);
        unify(tc, expected, ty_list(tc, elem));
        tc_comp(tc, e->a, &e->stmts, elem);
        return;
    }
    case EXPR_LEFT_SECTION: {
        Arg arg;
        arg.e = e->a;
        arg.n = NULL;
        set_pos(tc, e->name.span);
        tc_spine(tc, name_type(tc, &e->name), &arg, 1, expected);
        return;
    }
    case EXPR_RIGHT_SECTION: {
        /* (op x) is \y -> op y x. */
        Ty *a;
        Ty *rest;
        Ty *b;
        Ty *c;
        set_pos(tc, e->name.span);
        split_fun(tc, name_type(tc, &e->name), &a, &rest);
        split_fun(tc, rest, &b, &c);
        unify(tc, expected, ty_fun(tc, a, c));
        tc_expr_sigma(tc, e->a, b);
        return;
    }
    case EXPR_RECORD_CON:
        tc_record_con(tc, e, expected);
        return;
    case EXPR_RECORD_UPDATE:
        tc_record_update(tc, e, expected);
        return;
    case EXPR_SIG: {
        Ty *sigma = conv_sigma(tc, e->ty);
        if (prune(sigma)->kind != T_FORALL) {
            tc_expr(tc, e->a, sigma);
            unify(tc, sigma, expected);
            return;
        }
        tc_expr_sigma(tc, e->a, sigma);
        unify(tc, instantiate(tc, sigma, &enote(e)->evs), expected);
        return;
    }
    case EXPR_HOLE:
        tc_fail(tc, xstrdup("a hole `_` in an expression"));
        return;
    }
}

/* --- Binding groups ------------------------------------------------------------------ */

/* One binding of a group: the clauses of a function, or a pattern
 * binding. */
typedef struct {
    const char *name;   /* function bindings */
    Vec clauses;        /* Decl * */
    const Decl *pat;    /* pattern bindings */
    Vec vars;           /* char *: the variables of a pattern binding */
    Ty *sigma;          /* the signature, or NULL */
    Vec fv;             /* char *: the names the binding mentions */
    /* Tarjan. */
    int index;
    int low;
    int on_stack;
} Binding;

static void fv_expr(const Expr *e, Vec *out);
static void fv_decls(const Vec *decls, Vec *out);

static void fv_name(const QName *q, Vec *out)
{
    if (q->qual[0] == '\0') {
        vec_push(out, (void *)bare(q->name));
    }
}

static void fv_pat(const Pat *p, Vec *out)
{
    size_t i;
    if (p == NULL) {
        return;
    }
    switch (p->kind) {
    case PAT_VIEW:
        fv_expr(p->expr, out);
        fv_pat(p->a, out);
        break;
    case PAT_CON:
    case PAT_TUPLE:
    case PAT_LIST:
        for (i = 0; i < p->args.len; i++) {
            fv_pat((const Pat *)p->args.items[i], out);
        }
        break;
    case PAT_INFIX:
        fv_pat(p->a, out);
        for (i = 0; i < p->steps.len; i++) {
            fv_pat((const Pat *)((const InfixStep *)p->steps.items[i])->operand, out);
        }
        break;
    case PAT_RECORD:
        for (i = 0; i < p->fields.len; i++) {
            fv_pat(((const FieldPat *)p->fields.items[i])->pat, out);
        }
        break;
    case PAT_PAREN:
    case PAT_AS:
    case PAT_LAZY:
    case PAT_BANG:
    case PAT_SIG:
        fv_pat(p->a, out);
        break;
    default:
        break;
    }
}

static void fv_stmts(const Vec *stmts, Vec *out)
{
    size_t i;
    for (i = 0; i < stmts->len; i++) {
        const Stmt *s = (const Stmt *)stmts->items[i];
        if (s->expr != NULL) {
            fv_expr(s->expr, out);
        }
        fv_pat(s->pat, out);
        fv_decls(&s->decls, out);
    }
}

static void fv_rhs(const Rhs *rhs, Vec *out)
{
    size_t i;
    if (rhs == NULL) {
        return;
    }
    if (!rhs->guarded) {
        fv_expr(rhs->plain, out);
    } else {
        for (i = 0; i < rhs->guards.len; i++) {
            const GuardedRhs *g = (const GuardedRhs *)rhs->guards.items[i];
            fv_stmts(&g->guards, out);
            fv_expr(g->body, out);
        }
    }
    fv_decls(&rhs->wheres, out);
}

static void fv_decls(const Vec *decls, Vec *out)
{
    size_t i;
    size_t j;
    for (i = 0; i < decls->len; i++) {
        const Decl *d = (const Decl *)decls->items[i];
        if (d->kind != DECL_BIND) {
            continue;
        }
        fv_rhs(d->rhs, out);
        for (j = 0; j < d->lhs.args.len; j++) {
            fv_pat((const Pat *)d->lhs.args.items[j], out);
        }
        fv_pat(d->lhs.left, out);
        fv_pat(d->lhs.right, out);
        fv_pat(d->lhs.pat, out);
    }
}

static void fv_expr(const Expr *e, Vec *out)
{
    size_t i;
    if (e == NULL) {
        return;
    }
    switch (e->kind) {
    case EXPR_VAR:
        fv_name(&e->name, out);
        break;
    case EXPR_LEFT_SECTION:
    case EXPR_RIGHT_SECTION:
        fv_name(&e->name, out);
        fv_expr(e->a, out);
        break;
    case EXPR_INFIX:
        fv_expr(e->a, out);
        for (i = 0; i < e->steps.len; i++) {
            const InfixStep *s = (const InfixStep *)e->steps.items[i];
            fv_name(&s->op, out);
            fv_expr((const Expr *)s->operand, out);
        }
        break;
    case EXPR_LAMBDA:
        for (i = 0; i < e->pats.len; i++) {
            fv_pat((const Pat *)e->pats.items[i], out);
        }
        fv_expr(e->a, out);
        break;
    case EXPR_LAMBDA_CASE:
    case EXPR_CASE:
        fv_expr(e->a, out);
        for (i = 0; i < e->alts.len; i++) {
            const Alt *alt = (const Alt *)e->alts.items[i];
            fv_pat(alt->pat, out);
            fv_rhs(alt->rhs, out);
        }
        break;
    case EXPR_LET:
        fv_decls(&e->decls, out);
        fv_expr(e->a, out);
        break;
    case EXPR_DO:
    case EXPR_LIST_COMP:
        fv_stmts(&e->stmts, out);
        fv_expr(e->a, out);
        break;
    case EXPR_MULTI_IF:
        for (i = 0; i < e->guards.len; i++) {
            const GuardedRhs *g = (const GuardedRhs *)e->guards.items[i];
            fv_stmts(&g->guards, out);
            fv_expr(g->body, out);
        }
        break;
    case EXPR_TUPLE:
    case EXPR_TUPLE_SECTION:
    case EXPR_LIST:
        for (i = 0; i < e->items.len; i++) {
            fv_expr((const Expr *)e->items.items[i], out);
        }
        break;
    case EXPR_RECORD_CON:
    case EXPR_RECORD_UPDATE:
        fv_expr(e->a, out);
        for (i = 0; i < e->fields.len; i++) {
            const FieldUpdate *f = (const FieldUpdate *)e->fields.items[i];
            if (f->value != NULL) {
                fv_expr(f->value, out);
            } else {
                fv_name(&f->name, out);
            }
        }
        break;
    default:
        fv_expr(e->a, out);
        fv_expr(e->b, out);
        fv_expr(e->c, out);
        break;
    }
}

/* The variables that a pattern binds (char *). */
static void pat_binders(const Pat *p, Vec *out)
{
    size_t i;
    switch (p->kind) {
    case PAT_VAR:
        vec_push(out, p->var.name);
        break;
    case PAT_AS:
        vec_push(out, p->var.name);
        pat_binders(p->a, out);
        break;
    case PAT_CON:
    case PAT_TUPLE:
    case PAT_LIST:
        for (i = 0; i < p->args.len; i++) {
            pat_binders((const Pat *)p->args.items[i], out);
        }
        break;
    case PAT_INFIX:
        pat_binders(p->a, out);
        for (i = 0; i < p->steps.len; i++) {
            pat_binders((const Pat *)((const InfixStep *)p->steps.items[i])->operand, out);
        }
        break;
    case PAT_RECORD:
        for (i = 0; i < p->fields.len; i++) {
            const FieldPat *f = (const FieldPat *)p->fields.items[i];
            if (f->pat != NULL) {
                pat_binders(f->pat, out);
            } else {
                vec_push(out, (void *)bare(f->name.name));
            }
        }
        break;
    case PAT_PAREN:
    case PAT_LAZY:
    case PAT_BANG:
    case PAT_VIEW:
    case PAT_SIG:
        pat_binders(p->a, out);
        break;
    default:
        break;
    }
}

static size_t clause_arity(const Decl *d)
{
    if (d->lhs.kind == LHS_FUN) {
        return d->lhs.args.len;
    }
    return d->lhs.args.len + 2;
}

static const Pat *clause_arg(const Decl *d, size_t i)
{
    if (d->lhs.kind == LHS_FUN) {
        return (const Pat *)d->lhs.args.items[i];
    }
    if (i == 0) {
        return d->lhs.left;
    }
    if (i == 1) {
        return d->lhs.right;
    }
    return (const Pat *)d->lhs.args.items[i - 2];
}

/* Check the clauses of a function against a rho type. */
static void tc_clauses(Tc *tc, const Vec *clauses, Ty *ty)
{
    size_t i;
    size_t j;
    for (i = 0; i < clauses->len; i++) {
        const Decl *d = (const Decl *)clauses->items[i];
        size_t arity = clause_arity(d);
        size_t mark = tc->nlocals;
        PatGivens g;
        Ty *t = ty;
        tc->pos = d->pos;
        pg_init(&g);
        for (j = 0; j < arity; j++) {
            Ty *a;
            Ty *b;
            split_fun(tc, t, &a, &b);
            tc_pat(tc, clause_arg(d, j), a, &g);
            t = b;
        }
        if (g.preds.len > 0) {
            push_given_evs(tc, &g.preds, &g.evs);
        }
        tc_rhs(tc, d->rhs, t);
        if (g.preds.len > 0) {
            pop_givens(tc);
        }
        tc->nlocals = mark;
    }
}

void tc_check_binding(Tc *tc, const Vec *clauses, Ty *sigma, const char *name)
{
    Ty *t = prune(sigma);
    Vec dicts;
    size_t ntyvars = tc->tyvars.len;
    int pushed = 0;
    const Decl *first = (const Decl *)clauses->items[0];
    (void)name;
    vec_init(&dicts);
    tc->level++;
    while (t->kind == T_FORALL) {
        Vec preds;
        Vec sk;
        size_t i;
        vec_init(&sk);
        for (i = 0; i < t->vars.len; i++) {
            Ty *s = ty_skol(tc, ((Ty *)t->vars.items[i])->name);
            vec_push(&sk, s);
            if (t->scoped) {
                tyvar_push(tc, s->name, s);
            }
        }
        vec_init(&preds);
        for (i = 0; i < t->preds.len; i++) {
            vec_push(&preds, pred_subst((const TcPred *)t->preds.items[i], &t->vars, &sk));
        }
        if (preds.len > 0) {
            push_givens(tc, &preds, &dicts);
            pushed++;
        }
        t = prune(ty_subst(t->body, &t->vars, &sk));
    }
    tc_clauses(tc, clauses, t);
    solve(tc);
    while (pushed-- > 0) {
        pop_givens(tc);
    }
    tc->tyvars.len = ntyvars;
    tc->level--;
    if (dicts.len > 0) {
        TcNote *n = dnote(first);
        size_t i;
        for (i = 0; i < dicts.len; i++) {
            vec_push(&n->dicts, dicts.items[i]);
        }
    }
}

/* The binder's current type: a local, or a top-level value. A binder
 * of a group under inference has the group as mono; generalize sets
 * the final type of such a binder. */
static void set_binder_gen(Tc *tc, const char *name, Ty *type, MonoGroup *mono, int top,
                           int generalize)
{
    if (top) {
        TcVal *v = (TcVal *)strmap_get(&tc->values, tkey(tc->module, name));
        if (v == NULL) {
            v = (TcVal *)xcalloc(1, sizeof *v);
            strmap_put(&tc->values, tkey(tc->module, name), v);
        }
        v->type = type;
        v->mono = generalize ? NULL : mono;
        return;
    }
    {
        TcLocal *l = local_lookup_mut(tc, name);
        if (l != NULL && l->mono == mono && mono != NULL) {
            l->type = type;
            l->mono = NULL;
            return;
        }
        local_push(tc, name, type, generalize ? NULL : mono);
    }
}

static void set_binder(Tc *tc, const char *name, Ty *type, MonoGroup *mono, int top)
{
    set_binder_gen(tc, name, type, mono, top, 0);
}

/* Infer one strongly connected component of bindings without
 * signatures, then generalize it. */
static void infer_scc(Tc *tc, Binding **scc, size_t n, int top)
{
    MonoGroup *mg = (MonoGroup *)xcalloc(1, sizeof *mg);
    Ty **monos;
    Ty ***pat_monos;
    size_t i;
    size_t j;
    int restricted = 0;
    int outer = tc->level;
    Vec gen;
    Vec quantify;
    Vec rest;
    Vec vars;
    Vec preds;
    Vec dicts;
    Vec work;
    monos = (Ty **)xcalloc(n + 1, sizeof *monos);
    pat_monos = (Ty ***)xcalloc(n + 1, sizeof *pat_monos);
    vec_init(&mg->occs);
    /* Move the outer wanteds aside: only the component's wanteds take
     * part in its generalization. */
    work = tc->wanted;
    vec_init(&tc->wanted);
    tc->level++;
    for (i = 0; i < n; i++) {
        Binding *b = scc[i];
        if (b->pat != NULL) {
            restricted = 1;
            pat_monos[i] = (Ty **)xcalloc(b->vars.len + 1, sizeof(Ty *));
            for (j = 0; j < b->vars.len; j++) {
                pat_monos[i][j] = ty_meta(tc);
                set_binder(tc, (const char *)b->vars.items[j], pat_monos[i][j], mg, top);
            }
        } else {
            monos[i] = ty_meta(tc);
            if (clause_arity((const Decl *)b->clauses.items[0]) == 0) {
                restricted = 1;
            }
            set_binder(tc, b->name, monos[i], mg, top);
        }
    }
    for (i = 0; i < n; i++) {
        Binding *b = scc[i];
        if (b->pat != NULL) {
            Ty *t = ty_meta(tc);
            size_t mark = tc->nlocals;
            PatGivens g;
            tc->pos = b->pat->pos;
            tc_rhs(tc, b->pat->rhs, t);
            pg_init(&g);
            tc_pat(tc, b->pat->lhs.pat, t, &g);
            if (g.preds.len > 0) {
                tc_fail(tc, xstrdup("an existential constructor in a pattern binding"));
            }
            for (j = 0; j < b->vars.len; j++) {
                const TcLocal *l = local_lookup(tc, (const char *)b->vars.items[j]);
                unify(tc, l->type, pat_monos[i][j]);
            }
            tc->nlocals = mark;
        } else {
            tc_clauses(tc, &b->clauses, monos[i]);
        }
    }
    tc->level--;
    solve(tc);
    /* The meta variables to generalize. */
    vec_init(&gen);
    for (i = 0; i < n; i++) {
        if (monos[i] != NULL) {
            ty_metas(tc, monos[i], &gen);
        }
        if (pat_monos[i] != NULL) {
            for (j = 0; j < scc[i]->vars.len; j++) {
                ty_metas(tc, pat_monos[i][j], &gen);
            }
        }
    }
    for (i = 0; i < gen.len;) {
        if (((Ty *)gen.items[i])->level <= outer) {
            vec_remove(&gen, i);
        } else {
            i++;
        }
    }
    /* Ambiguous wanteds: inner metas that are not in the types. Default
     * them. */
    {
        Vec amb;
        vec_init(&amb);
        for (i = 0; i < tc->wanted.len; i++) {
            Wanted *w = (Wanted *)tc->wanted.items[i];
            Vec ms;
            int ambiguous = 0;
            vec_init(&ms);
            pred_metas(tc, w->pred, &ms);
            for (j = 0; j < ms.len; j++) {
                Ty *m = (Ty *)ms.items[j];
                size_t k;
                int in_gen = 0;
                if (m->level <= outer) {
                    continue;
                }
                for (k = 0; k < gen.len; k++) {
                    if (gen.items[k] == m) {
                        in_gen = 1;
                    }
                }
                if (!in_gen) {
                    ambiguous = 1;
                }
            }
            if (ambiguous) {
                vec_push(&amb, w);
            }
        }
        if (amb.len > 0 && apply_defaults(tc, &amb)) {
            solve(tc);
        }
    }
    vec_init(&quantify);
    vec_init(&rest);
    for (i = 0; i < tc->wanted.len; i++) {
        Wanted *w = (Wanted *)tc->wanted.items[i];
        Vec ms;
        int inner = 0;
        vec_init(&ms);
        pred_metas(tc, w->pred, &ms);
        for (j = 0; j < ms.len; j++) {
            Ty *m = (Ty *)ms.items[j];
            size_t k;
            for (k = 0; k < gen.len; k++) {
                if (gen.items[k] == m) {
                    inner = 1;
                }
            }
        }
        if (inner && restricted) {
            /* The monomorphism restriction: the constrained variables
             * stay meta variables of the outer level. */
            for (j = 0; j < ms.len; j++) {
                Ty *m = (Ty *)ms.items[j];
                size_t k;
                if (m->level > outer) {
                    m->level = outer;
                }
                for (k = 0; k < gen.len; k++) {
                    if (gen.items[k] == m) {
                        vec_remove(&gen, k);
                        break;
                    }
                }
            }
            vec_push(&rest, w);
        } else if (inner && w->pred->cls != NULL) {
            vec_push(&quantify, w);
        } else {
            vec_push(&rest, w);
        }
    }
    /* Metas that the restriction lowered may be in other wanteds of the
     * quantify list; move those back. */
    if (restricted) {
        for (i = 0; i < quantify.len;) {
            Wanted *w = (Wanted *)quantify.items[i];
            Vec ms;
            int inner = 0;
            vec_init(&ms);
            pred_metas(tc, w->pred, &ms);
            for (j = 0; j < ms.len; j++) {
                if (((Ty *)ms.items[j])->level > outer) {
                    inner = 1;
                }
            }
            if (!inner) {
                vec_remove(&quantify, i);
                vec_push(&rest, w);
            } else {
                i++;
            }
        }
    }
    /* Bind the variables. */
    vec_init(&vars);
    for (i = 0; i < gen.len; i++) {
        Ty *m = (Ty *)gen.items[i];
        Ty *b;
        if (prune(m) != m || m->level <= outer) {
            continue;
        }
        b = ty_bound(tc, str_printf("t%lu", (unsigned long)vars.len + 1));
        m->ref = b;
        vec_push(&vars, b);
    }
    /* The dictionary parameters. */
    vec_init(&preds);
    vec_init(&dicts);
    for (i = 0; i < quantify.len; i++) {
        Wanted *w = (Wanted *)quantify.items[i];
        TcEv *ev = NULL;
        for (j = 0; j < preds.len; j++) {
            if (pred_equal(tc, (const TcPred *)preds.items[j], w->pred)) {
                ev = ev_new(EV_VAR);
                ev->var = (const char *)dicts.items[j];
            }
        }
        if (ev == NULL) {
            ev = ev_new(EV_VAR);
            ev->var = fresh_dict(tc);
            vec_push(&preds, w->pred);
            vec_push(&dicts, (void *)ev->var);
        }
        w->ev->ref = ev;
    }
    for (i = 0; i < mg->occs.len; i++) {
        TcNote *note = (TcNote *)mg->occs.items[i];
        for (j = 0; j < dicts.len; j++) {
            TcEv *ev = ev_new(EV_VAR);
            ev->var = (const char *)dicts.items[j];
            vec_push(&note->evs, ev);
        }
    }
    for (i = 0; i < n; i++) {
        Binding *b = scc[i];
        if (b->pat != NULL) {
            for (j = 0; j < b->vars.len; j++) {
                Vec none;
                vec_init(&none);
                set_binder_gen(tc, (const char *)b->vars.items[j],
                               ty_forall(&vars, &none, pat_monos[i][j]), mg, top, 1);
            }
        } else {
            set_binder_gen(tc, b->name, ty_forall(&vars, &preds, monos[i]), mg, top, 1);
            if (dicts.len > 0) {
                TcNote *note = dnote((const Decl *)b->clauses.items[0]);
                for (j = 0; j < dicts.len; j++) {
                    vec_push(&note->dicts, dicts.items[j]);
                }
            }
        }
    }
    /* The remaining wanteds float out. */
    for (i = 0; i < rest.len; i++) {
        vec_push(&work, rest.items[i]);
    }
    tc->wanted = work;
}

typedef struct {
    Binding **items;
    size_t n;
    Binding **stack;
    size_t sp;
    int index;
    Tc *tc;
    int top;
} Tarjan;

static int binding_mentions(const Binding *from, const Binding *to)
{
    size_t i;
    for (i = 0; i < from->fv.len; i++) {
        const char *name = (const char *)from->fv.items[i];
        if (to->pat == NULL) {
            if (str_eq(name, to->name)) {
                return 1;
            }
        } else if (vec_has_str(&to->vars, name)) {
            return 1;
        }
    }
    return 0;
}

static void strongconnect(Tarjan *t, Binding *v)
{
    size_t i;
    v->index = t->index;
    v->low = t->index;
    t->index++;
    t->stack[t->sp++] = v;
    v->on_stack = 1;
    for (i = 0; i < t->n; i++) {
        Binding *w = t->items[i];
        if (!binding_mentions(v, w)) {
            continue;
        }
        if (w->index < 0) {
            strongconnect(t, w);
            if (w->low < v->low) {
                v->low = w->low;
            }
        } else if (w->on_stack && w->index < v->low) {
            v->low = w->index;
        }
    }
    if (v->low == v->index) {
        Binding **scc = (Binding **)xmalloc((t->sp + 1) * sizeof *scc);
        size_t n = 0;
        Binding *w;
        do {
            w = t->stack[--t->sp];
            w->on_stack = 0;
            scc[n++] = w;
        } while (w != v);
        infer_scc(t->tc, scc, n, t->top);
    }
}

void tc_bind_group(Tc *tc, const Vec *decls, int top)
{
    Vec bindings;
    StrMap sigs;
    size_t i;
    size_t j;
    Vec unsigned_;
    vec_init(&bindings);
    strmap_init(&sigs);
    for (i = 0; i < decls->len; i++) {
        const Decl *d = (const Decl *)decls->items[i];
        if (d->kind == DECL_TYPE_SIG) {
            Ty *sigma;
            tc->pos = d->pos;
            sigma = conv_sigma(tc, d->ty);
            for (j = 0; j < d->names.len; j++) {
                strmap_put(&sigs, bare(((const Ident *)d->names.items[j])->name), sigma);
            }
        }
    }
    for (i = 0; i < decls->len; i++) {
        const Decl *d = (const Decl *)decls->items[i];
        Binding *b;
        const char *name;
        if (d->kind != DECL_BIND) {
            if (!top && d->kind != DECL_TYPE_SIG && d->kind != DECL_FIXITY) {
                tc->pos = d->pos;
                tc_fail(tc, xstrdup("this local declaration is not supported yet"));
            }
            continue;
        }
        if (d->lhs.kind == LHS_PAT) {
            const Pat *p = d->lhs.pat;
            while (p->kind == PAT_PAREN || p->kind == PAT_BANG) {
                p = p->a;
            }
            if (p->kind == PAT_VAR) {
                /* `x = e` with a bang or parentheses: a simple binding. */
                b = (Binding *)xcalloc(1, sizeof *b);
                b->name = p->var.name;
                vec_init(&b->clauses);
                b->pat = d;
                vec_init(&b->vars);
                vec_push(&b->vars, (void *)p->var.name);
                b->index = -1;
                vec_push(&bindings, b);
                continue;
            }
            b = (Binding *)xcalloc(1, sizeof *b);
            b->pat = d;
            vec_init(&b->vars);
            pat_binders(d->lhs.pat, &b->vars);
            b->index = -1;
            vec_push(&bindings, b);
            continue;
        }
        name = bare(d->lhs.name.name);
        if (bindings.len > 0) {
            b = (Binding *)vec_last(&bindings);
            if (b->pat == NULL && str_eq(b->name, name)) {
                vec_push(&b->clauses, (void *)d);
                continue;
            }
        }
        b = (Binding *)xcalloc(1, sizeof *b);
        b->name = name;
        vec_init(&b->clauses);
        vec_push(&b->clauses, (void *)d);
        b->index = -1;
        vec_push(&bindings, b);
    }
    /* Signatures first, so every binding sees them. */
    vec_init(&unsigned_);
    for (i = 0; i < bindings.len; i++) {
        Binding *b = (Binding *)bindings.items[i];
        if (b->pat == NULL) {
            b->sigma = (Ty *)strmap_get(&sigs, b->name);
        } else if (b->vars.len == 1 && b->clauses.len == 0) {
            b->sigma = (Ty *)strmap_get(&sigs, (const char *)b->vars.items[0]);
        }
        if (b->sigma != NULL) {
            set_binder(tc, b->pat == NULL ? b->name : (const char *)b->vars.items[0], b->sigma,
                       NULL, top);
        } else {
            if (b->pat != NULL) {
                for (j = 0; j < b->vars.len; j++) {
                    Ty *s = (Ty *)strmap_get(&sigs, (const char *)b->vars.items[j]);
                    if (s != NULL) {
                        tc->pos = b->pat->pos;
                        tc_fail(tc, xstrdup("a signature for a variable of a pattern binding is "
                                            "not supported yet"));
                    }
                }
            }
            vec_init(&b->fv);
            if (b->pat != NULL) {
                fv_rhs(b->pat->rhs, &b->fv);
            } else {
                fv_decls(&b->clauses, &b->fv);
            }
            vec_push(&unsigned_, b);
        }
    }
    if (unsigned_.len > 0) {
        Tarjan t;
        t.items = (Binding **)unsigned_.items;
        t.n = unsigned_.len;
        t.stack = (Binding **)xmalloc((unsigned_.len + 1) * sizeof(Binding *));
        t.sp = 0;
        t.index = 0;
        t.tc = tc;
        t.top = top;
        for (i = 0; i < unsigned_.len; i++) {
            Binding *b = (Binding *)unsigned_.items[i];
            if (b->index < 0) {
                strongconnect(&t, b);
            }
        }
    }
    for (i = 0; i < bindings.len; i++) {
        Binding *b = (Binding *)bindings.items[i];
        if (b->sigma == NULL) {
            continue;
        }
        if (b->pat != NULL) {
            /* `x = e` or `!x = e` with a signature. */
            Vec clauses;
            Decl *fake;
            vec_init(&clauses);
            fake = decl_new(DECL_BIND, b->pat->pos);
            fake->lhs.kind = LHS_FUN;
            fake->lhs.name.name = (char *)b->vars.items[0];
            vec_init(&fake->lhs.args);
            fake->rhs = b->pat->rhs;
            vec_push(&clauses, fake);
            tc_check_binding(tc, &clauses, b->sigma, b->name);
            if (fake->note != NULL && fake->note->dicts.len > 0) {
                ((Decl *)b->pat)->note = fake->note;
            }
            continue;
        }
        tc_check_binding(tc, &b->clauses, b->sigma, b->name);
    }
}
