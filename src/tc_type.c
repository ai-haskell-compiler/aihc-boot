/* Types, substitution, unification and matching. See tc_int.h. */
#include <string.h>

#include "tc_int.h"

void tc_fail(Tc *tc, char *message)
{
    tc->error = message;
    longjmp(tc->jump, 1);
}

char *tkey(const char *module, const char *name)
{
    return str_printf("%s\t%s", module, name);
}

/* --- Construction --------------------------------------------------------- */

Ty *ty_new(TyKind kind)
{
    Ty *t = (Ty *)xcalloc(1, sizeof *t);
    t->kind = kind;
    return t;
}

Ty *ty_meta(Tc *tc)
{
    Ty *t = ty_new(T_META);
    t->level = tc->level;
    t->id = ++tc->next_id;
    return t;
}

Ty *ty_skol(Tc *tc, const char *name)
{
    Ty *t = ty_new(T_SKOL);
    t->level = tc->level;
    t->id = ++tc->next_id;
    t->name = name;
    return t;
}

Ty *ty_bound(Tc *tc, const char *name)
{
    Ty *t = ty_new(T_BOUND);
    t->id = ++tc->next_id;
    t->name = name;
    return t;
}

Ty *ty_con(TcTyCon *con)
{
    Ty *t = ty_new(T_CON);
    t->con = con;
    return t;
}

Ty *ty_app(Ty *f, Ty *a)
{
    Ty *t = ty_new(T_APP);
    t->fun = f;
    t->arg = a;
    return t;
}

Ty *ty_fun(Tc *tc, Ty *a, Ty *b)
{
    return ty_app(ty_app(ty_con(tc->tc_fun), a), b);
}

Ty *ty_apps(Ty *f, const Vec *args)
{
    size_t i;
    for (i = 0; i < args->len; i++) {
        f = ty_app(f, (Ty *)args->items[i]);
    }
    return f;
}

Ty *ty_forall(const Vec *vars, const Vec *preds, Ty *body)
{
    Ty *t;
    if (vars->len == 0 && preds->len == 0) {
        return body;
    }
    t = ty_new(T_FORALL);
    t->vars = *vars;
    t->preds = *preds;
    t->body = body;
    return t;
}

TcPred *pred_new(TcClass *cls, const Vec *args)
{
    TcPred *p = (TcPred *)xcalloc(1, sizeof *p);
    p->cls = cls;
    if (args != NULL) {
        p->args = *args;
    }
    return p;
}

Ty *prune(Ty *t)
{
    Ty *root = t;
    while (root->kind == T_META && root->ref != NULL) {
        root = root->ref;
    }
    while (t->kind == T_META && t->ref != NULL && t->ref != root) {
        Ty *next = t->ref;
        t->ref = root;
        t = next;
    }
    return root;
}

Ty *ty_spine(Ty *t, Vec *args)
{
    vec_init(args);
    t = prune(t);
    while (t->kind == T_APP) {
        vec_insert(args, 0, t->arg);
        t = prune(t->fun);
    }
    return t;
}

/* --- Substitution ----------------------------------------------------------- */

static Ty *subst_lookup(Ty *t, const Vec *from, const Vec *to)
{
    size_t i;
    for (i = 0; i < from->len; i++) {
        if (((Ty *)from->items[i])->id == t->id) {
            return (Ty *)to->items[i];
        }
    }
    return NULL;
}

Ty *ty_subst(Ty *t, const Vec *from, const Vec *to)
{
    Ty *r;
    size_t i;
    t = prune(t);
    switch (t->kind) {
    case T_BOUND:
        r = subst_lookup(t, from, to);
        return r != NULL ? r : t;
    case T_APP: {
        Ty *f = ty_subst(t->fun, from, to);
        Ty *a = ty_subst(t->arg, from, to);
        if (f == t->fun && a == t->arg) {
            return t;
        }
        return ty_app(f, a);
    }
    case T_FAM:
        r = ty_new(T_FAM);
        r->con = t->con;
        vec_init(&r->args);
        for (i = 0; i < t->args.len; i++) {
            vec_push(&r->args, ty_subst((Ty *)t->args.items[i], from, to));
        }
        return r;
    case T_FORALL: {
        Vec preds;
        vec_init(&preds);
        for (i = 0; i < t->preds.len; i++) {
            vec_push(&preds, pred_subst((const TcPred *)t->preds.items[i], from, to));
        }
        r = ty_new(T_FORALL);
        r->vars = t->vars;
        r->preds = preds;
        r->body = ty_subst(t->body, from, to);
        return r;
    }
    default:
        return t;
    }
}

TcPred *pred_subst(const TcPred *p, const Vec *from, const Vec *to)
{
    TcPred *r = (TcPred *)xcalloc(1, sizeof *r);
    size_t i;
    r->cls = p->cls;
    vec_init(&r->args);
    for (i = 0; i < p->args.len; i++) {
        vec_push(&r->args, ty_subst((Ty *)p->args.items[i], from, to));
    }
    if (p->a != NULL) {
        r->a = ty_subst(p->a, from, to);
        r->b = ty_subst(p->b, from, to);
    }
    return r;
}

Ty *zonk(Tc *tc, Ty *t)
{
    Ty *r;
    size_t i;
    t = ty_whnf(tc, t);
    switch (t->kind) {
    case T_APP:
        return ty_app(zonk(tc, t->fun), zonk(tc, t->arg));
    case T_FAM:
        r = ty_new(T_FAM);
        r->con = t->con;
        for (i = 0; i < t->args.len; i++) {
            vec_push(&r->args, zonk(tc, (Ty *)t->args.items[i]));
        }
        return r;
    case T_FORALL:
        r = ty_new(T_FORALL);
        r->vars = t->vars;
        for (i = 0; i < t->preds.len; i++) {
            vec_push(&r->preds, pred_zonk(tc, (const TcPred *)t->preds.items[i]));
        }
        r->body = zonk(tc, t->body);
        return r;
    default:
        return t;
    }
}

TcPred *pred_zonk(Tc *tc, const TcPred *p)
{
    TcPred *r = (TcPred *)xcalloc(1, sizeof *r);
    size_t i;
    r->cls = p->cls;
    for (i = 0; i < p->args.len; i++) {
        vec_push(&r->args, zonk(tc, (Ty *)p->args.items[i]));
    }
    if (p->a != NULL) {
        r->a = zonk(tc, p->a);
        r->b = zonk(tc, p->b);
    }
    return r;
}

/* --- Type families -------------------------------------------------------- */

Ty *ty_whnf(Tc *tc, Ty *t)
{
    size_t i;
    size_t j;
    int steps = 0;
    for (;;) {
        int reduced = 0;
        t = prune(t);
        if (t->kind != T_FAM) {
            return t;
        }
        for (i = 0; i < t->con->eqns.len && !reduced; i++) {
            const TcFamEqn *eq = (const TcFamEqn *)t->con->eqns.items[i];
            Vec to;
            int ok = 1;
            vec_init(&to);
            for (j = 0; j < eq->vars.len; j++) {
                vec_push(&to, NULL);
            }
            for (j = 0; j < eq->lhs.len && ok; j++) {
                ok = ty_match(tc, (Ty *)eq->lhs.items[j], (Ty *)t->args.items[j], &eq->vars, &to);
            }
            if (ok) {
                for (j = 0; j < to.len; j++) {
                    if (to.items[j] == NULL) {
                        to.items[j] = ty_meta(tc);
                    }
                }
                t = ty_subst(eq->rhs, &eq->vars, &to);
                reduced = 1;
            }
        }
        if (!reduced) {
            return t;
        }
        if (++steps > 1000) {
            tc_fail(tc, xstrdup("type family reduction does not terminate"));
        }
    }
}

int ty_is_fun(Tc *tc, Ty *t, Ty **a, Ty **b)
{
    Ty *f;
    t = ty_whnf(tc, t);
    if (t->kind != T_APP) {
        return 0;
    }
    f = ty_whnf(tc, t->fun);
    if (f->kind != T_APP) {
        return 0;
    }
    if (ty_whnf(tc, f->fun)->kind != T_CON || ty_whnf(tc, f->fun)->con != tc->tc_fun) {
        return 0;
    }
    *a = f->arg;
    *b = t->arg;
    return 1;
}

/* --- Unification ----------------------------------------------------------- */

static int occurs(Tc *tc, Ty *meta, Ty *t, int level)
{
    size_t i;
    t = prune(t);
    switch (t->kind) {
    case T_META:
        if (t == meta) {
            return 1;
        }
        if (t->level > level) {
            t->level = level;
        }
        return 0;
    case T_APP:
        return occurs(tc, meta, t->fun, level) || occurs(tc, meta, t->arg, level);
    case T_FAM:
        for (i = 0; i < t->args.len; i++) {
            if (occurs(tc, meta, (Ty *)t->args.items[i], level)) {
                return 1;
            }
        }
        return 0;
    case T_FORALL:
        for (i = 0; i < t->preds.len; i++) {
            const TcPred *p = (const TcPred *)t->preds.items[i];
            size_t j;
            for (j = 0; j < p->args.len; j++) {
                if (occurs(tc, meta, (Ty *)p->args.items[j], level)) {
                    return 1;
                }
            }
        }
        return occurs(tc, meta, t->body, level);
    default:
        return 0;
    }
}

static int unify_rec(Tc *tc, Ty *a, Ty *b);

static int bind_meta(Tc *tc, Ty *m, Ty *t)
{
    if (t->kind == T_META && t == m) {
        return 0;
    }
    if (occurs(tc, m, t, m->level)) {
        /* A family application may reduce away the occurrence. */
        Ty *w = ty_whnf(tc, t);
        if (w != t) {
            return unify_rec(tc, m, w);
        }
        return 1;
    }
    m->ref = t;
    return 0;
}

static int unify_rec(Tc *tc, Ty *a, Ty *b)
{
    size_t i;
    a = ty_whnf(tc, a);
    b = ty_whnf(tc, b);
    if (a == b) {
        return 0;
    }
    if (a->kind == T_META) {
        if (b->kind == T_META && b->level > a->level) {
            return bind_meta(tc, b, a);
        }
        return bind_meta(tc, a, b);
    }
    if (b->kind == T_META) {
        return bind_meta(tc, b, a);
    }
    if (a->kind == T_FAM || b->kind == T_FAM) {
        if (ty_equal(tc, a, b)) {
            return 0;
        }
        emit_equality(tc, a, b);
        return 0;
    }
    if (a->kind != b->kind) {
        return 1;
    }
    switch (a->kind) {
    case T_CON:
        return a->con != b->con;
    case T_APP:
        if (unify_rec(tc, a->fun, b->fun) != 0) {
            return 1;
        }
        return unify_rec(tc, a->arg, b->arg);
    case T_SKOL:
    case T_BOUND:
        return a->id != b->id;
    case T_FORALL: {
        Vec sk;
        Ty *ba;
        Ty *bb;
        if (a->vars.len != b->vars.len) {
            return 1;
        }
        vec_init(&sk);
        for (i = 0; i < a->vars.len; i++) {
            vec_push(&sk, ty_skol(tc, ((Ty *)a->vars.items[i])->name));
        }
        ba = ty_subst(a->body, &a->vars, &sk);
        bb = ty_subst(b->body, &b->vars, &sk);
        return unify_rec(tc, ba, bb);
    }
    default:
        return 1;
    }
}

int try_unify(Tc *tc, Ty *a, Ty *b)
{
    return unify_rec(tc, a, b);
}

void unify(Tc *tc, Ty *a, Ty *b)
{
    if (unify_rec(tc, a, b) != 0) {
        Ty *za = zonk(tc, a);
        Ty *zb = zonk(tc, b);
        tc_fail(tc, str_printf("cannot match the type %s with %s", ty_str(za), ty_str(zb)));
    }
}

int ty_equal(Tc *tc, Ty *a, Ty *b)
{
    size_t i;
    a = ty_whnf(tc, a);
    b = ty_whnf(tc, b);
    if (a == b) {
        return 1;
    }
    if (a->kind != b->kind) {
        return 0;
    }
    switch (a->kind) {
    case T_CON:
        return a->con == b->con;
    case T_APP:
        return ty_equal(tc, a->fun, b->fun) && ty_equal(tc, a->arg, b->arg);
    case T_SKOL:
    case T_BOUND:
        return a->id == b->id;
    case T_FAM:
        if (a->con != b->con || a->args.len != b->args.len) {
            return 0;
        }
        for (i = 0; i < a->args.len; i++) {
            if (!ty_equal(tc, (Ty *)a->args.items[i], (Ty *)b->args.items[i])) {
                return 0;
            }
        }
        return 1;
    default:
        return 0;
    }
}

int pred_equal(Tc *tc, const TcPred *a, const TcPred *b)
{
    size_t i;
    if (a->cls != b->cls || a->args.len != b->args.len) {
        return 0;
    }
    for (i = 0; i < a->args.len; i++) {
        if (!ty_equal(tc, (Ty *)a->args.items[i], (Ty *)b->args.items[i])) {
            return 0;
        }
    }
    if (a->cls == NULL) {
        return ty_equal(tc, a->a, b->a) && ty_equal(tc, a->b, b->b);
    }
    return 1;
}

int ty_match(Tc *tc, Ty *pat, Ty *t, const Vec *vars, Vec *to)
{
    size_t i;
    pat = prune(pat);
    if (pat->kind == T_BOUND) {
        for (i = 0; i < vars->len; i++) {
            if (((Ty *)vars->items[i])->id == pat->id) {
                if (to->items[i] == NULL) {
                    to->items[i] = prune(t);
                    return 1;
                }
                return ty_equal(tc, (Ty *)to->items[i], t);
            }
        }
    }
    t = ty_whnf(tc, t);
    if (pat->kind != t->kind) {
        return 0;
    }
    switch (pat->kind) {
    case T_CON:
        return pat->con == t->con;
    case T_APP:
        return ty_match(tc, pat->fun, t->fun, vars, to) && ty_match(tc, pat->arg, t->arg, vars, to);
    case T_SKOL:
    case T_BOUND:
        return pat->id == t->id;
    case T_FAM:
        if (pat->con != t->con) {
            return 0;
        }
        for (i = 0; i < pat->args.len; i++) {
            if (!ty_match(tc, (Ty *)pat->args.items[i], (Ty *)t->args.items[i], vars, to)) {
                return 0;
            }
        }
        return 1;
    default:
        return 0;
    }
}

/* --- Free variables --------------------------------------------------------- */

void ty_metas(Tc *tc, Ty *t, Vec *out)
{
    size_t i;
    t = ty_whnf(tc, t);
    switch (t->kind) {
    case T_META:
        for (i = 0; i < out->len; i++) {
            if (out->items[i] == t) {
                return;
            }
        }
        vec_push(out, t);
        return;
    case T_APP:
        ty_metas(tc, t->fun, out);
        ty_metas(tc, t->arg, out);
        return;
    case T_FAM:
        for (i = 0; i < t->args.len; i++) {
            ty_metas(tc, (Ty *)t->args.items[i], out);
        }
        return;
    case T_FORALL:
        for (i = 0; i < t->preds.len; i++) {
            pred_metas(tc, (const TcPred *)t->preds.items[i], out);
        }
        ty_metas(tc, t->body, out);
        return;
    default:
        return;
    }
}

void pred_metas(Tc *tc, const TcPred *p, Vec *out)
{
    size_t i;
    for (i = 0; i < p->args.len; i++) {
        ty_metas(tc, (Ty *)p->args.items[i], out);
    }
    if (p->a != NULL) {
        ty_metas(tc, p->a, out);
        ty_metas(tc, p->b, out);
    }
}

int ty_has_skolem(Tc *tc, Ty *t, const Vec *skolems)
{
    size_t i;
    t = ty_whnf(tc, t);
    switch (t->kind) {
    case T_SKOL:
        for (i = 0; i < skolems->len; i++) {
            if (((Ty *)skolems->items[i])->id == t->id) {
                return 1;
            }
        }
        return 0;
    case T_APP:
        return ty_has_skolem(tc, t->fun, skolems) || ty_has_skolem(tc, t->arg, skolems);
    case T_FAM:
        for (i = 0; i < t->args.len; i++) {
            if (ty_has_skolem(tc, (Ty *)t->args.items[i], skolems)) {
                return 1;
            }
        }
        return 0;
    case T_FORALL:
        return ty_has_skolem(tc, t->body, skolems);
    default:
        return 0;
    }
}

/* --- Instantiation ------------------------------------------------------------ */

Ty *instantiate_with(Tc *tc, Ty *t, const Vec *types, Vec *evs)
{
    size_t i;
    Ty *body;
    t = prune(t);
    if (t->kind != T_FORALL) {
        return t;
    }
    for (i = 0; i < t->preds.len; i++) {
        TcPred *p = pred_subst((const TcPred *)t->preds.items[i], &t->vars, types);
        if (p->cls == NULL) {
            unify(tc, p->a, p->b);
        } else {
            TcEv *hole = emit_wanted(tc, p);
            if (evs != NULL) {
                vec_push(evs, hole);
            }
        }
    }
    body = ty_subst(t->body, &t->vars, types);
    return body;
}

Ty *instantiate(Tc *tc, Ty *t, Vec *evs)
{
    for (;;) {
        Vec metas;
        size_t i;
        t = prune(t);
        if (t->kind != T_FORALL) {
            return t;
        }
        vec_init(&metas);
        for (i = 0; i < t->vars.len; i++) {
            vec_push(&metas, ty_meta(tc));
        }
        t = instantiate_with(tc, t, &metas, evs);
    }
}

/* --- Printing ------------------------------------------------------------------ */

static void print_prec(StrBuf *b, Ty *t, int prec);

static void print_pred(StrBuf *b, const TcPred *p)
{
    size_t i;
    if (p->cls == NULL) {
        print_prec(b, p->a, 1);
        sb_puts(b, " ~ ");
        print_prec(b, p->b, 1);
        return;
    }
    sb_puts(b, p->cls->name);
    for (i = 0; i < p->args.len; i++) {
        sb_putc(b, ' ');
        print_prec(b, (Ty *)p->args.items[i], 2);
    }
}

static void print_prec(StrBuf *b, Ty *t, int prec)
{
    Vec args;
    Ty *head;
    size_t i;
    t = prune(t);
    switch (t->kind) {
    case T_META:
        sb_printf(b, "t%lu", t->id);
        return;
    case T_SKOL:
    case T_BOUND:
        sb_puts(b, t->name != NULL ? t->name : "a");
        return;
    case T_CON:
        sb_puts(b, t->con->name);
        return;
    case T_FAM:
        if (prec > 1 && t->args.len > 0) {
            sb_putc(b, '(');
        }
        sb_puts(b, t->con->name);
        for (i = 0; i < t->args.len; i++) {
            sb_putc(b, ' ');
            print_prec(b, (Ty *)t->args.items[i], 2);
        }
        if (prec > 1 && t->args.len > 0) {
            sb_putc(b, ')');
        }
        return;
    case T_FORALL:
        if (prec > 0) {
            sb_putc(b, '(');
        }
        sb_puts(b, "forall");
        for (i = 0; i < t->vars.len; i++) {
            sb_putc(b, ' ');
            print_prec(b, (Ty *)t->vars.items[i], 2);
        }
        sb_puts(b, ". ");
        if (t->preds.len > 0) {
            sb_putc(b, '(');
            for (i = 0; i < t->preds.len; i++) {
                if (i > 0) {
                    sb_puts(b, ", ");
                }
                print_pred(b, (const TcPred *)t->preds.items[i]);
            }
            sb_puts(b, ") => ");
        }
        print_prec(b, t->body, 0);
        if (prec > 0) {
            sb_putc(b, ')');
        }
        return;
    case T_APP:
        head = ty_spine(t, &args);
        if (head->kind == T_CON && str_eq(head->con->name, "->") && args.len == 2) {
            if (prec > 0) {
                sb_putc(b, '(');
            }
            print_prec(b, (Ty *)args.items[0], 1);
            sb_puts(b, " -> ");
            print_prec(b, (Ty *)args.items[1], 0);
            if (prec > 0) {
                sb_putc(b, ')');
            }
            return;
        }
        if (head->kind == T_CON && str_eq(head->con->name, "List") && args.len == 1) {
            sb_putc(b, '[');
            print_prec(b, (Ty *)args.items[0], 0);
            sb_putc(b, ']');
            return;
        }
        if (head->kind == T_CON && str_starts_with(head->con->name, "(,")
            && (size_t)head->con->arity == args.len) {
            sb_putc(b, '(');
            for (i = 0; i < args.len; i++) {
                if (i > 0) {
                    sb_puts(b, ", ");
                }
                print_prec(b, (Ty *)args.items[i], 0);
            }
            sb_putc(b, ')');
            return;
        }
        if (prec > 1) {
            sb_putc(b, '(');
        }
        print_prec(b, head, 2);
        for (i = 0; i < args.len; i++) {
            sb_putc(b, ' ');
            print_prec(b, (Ty *)args.items[i], 2);
        }
        if (prec > 1) {
            sb_putc(b, ')');
        }
        return;
    }
}

void ty_print(StrBuf *b, Ty *t)
{
    print_prec(b, t, 0);
}

char *ty_str(Ty *t)
{
    StrBuf b;
    sb_init(&b);
    ty_print(&b, t);
    return sb_take(&b);
}

char *ty_str_atom(Ty *t)
{
    StrBuf b;
    sb_init(&b);
    print_prec(&b, t, 2);
    return sb_take(&b);
}

char *pred_str(const TcPred *p)
{
    StrBuf b;
    sb_init(&b);
    print_pred(&b, p);
    return sb_take(&b);
}
