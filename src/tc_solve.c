/* Constraints and the solver. See tc_int.h.
 *
 * A wanted constraint keeps the givens that were in scope where it was
 * made. So a wanted that floats out of a binding with a signature can
 * still use the givens of the signature when the solver gets to it
 * later.
 */
#include <string.h>

#include "tc_int.h"

TcEv *ev_new(TcEvKind kind)
{
    TcEv *e = (TcEv *)xcalloc(1, sizeof *e);
    e->kind = kind;
    vec_init(&e->args);
    return e;
}

TcEv *tc_ev(TcEv *e)
{
    while (e->kind == EV_HOLE && e->ref != NULL) {
        e = e->ref;
    }
    return e;
}

TcNote *tc_note_new(void)
{
    TcNote *n = (TcNote *)xcalloc(1, sizeof *n);
    vec_init(&n->evs);
    vec_init(&n->dicts);
    vec_init(&n->lams);
    return n;
}

char *tc_default_name(const char *method)
{
    return str_printf("%s{default}", method);
}

char *fresh_dict(Tc *tc)
{
    return str_printf("$d%lu", ++tc->next_dict);
}

static Wanted *wanted_new(Tc *tc, TcPred *p)
{
    Wanted *w = (Wanted *)xcalloc(1, sizeof *w);
    w->pred = p;
    w->givens = tc->givens;
    w->pos = tc->pos;
    w->path = tc->path;
    w->level = tc->level;
    return w;
}

TcEv *emit_wanted(Tc *tc, TcPred *p)
{
    Wanted *w = wanted_new(tc, p);
    w->ev = ev_new(EV_HOLE);
    vec_push(&tc->wanted, w);
    return w->ev;
}

void emit_equality(Tc *tc, Ty *a, Ty *b)
{
    TcPred *p = pred_new(NULL, NULL);
    Wanted *w;
    p->a = a;
    p->b = b;
    w = wanted_new(tc, p);
    vec_push(&tc->wanted, w);
}

/* --- Givens ------------------------------------------------------------------ */

static void add_given(Tc *tc, Givens *g, TcPred *p, TcEv *ev, int depth)
{
    size_t i;
    if (depth > 20) {
        return;
    }
    for (i = 0; i < g->preds.len; i++) {
        if (pred_equal(tc, (const TcPred *)g->preds.items[i], p)) {
            return;
        }
    }
    vec_push(&g->preds, p);
    vec_push(&g->evs, ev);
    if (p->cls == NULL) {
        return;
    }
    for (i = 0; i < p->cls->supers.len; i++) {
        TcPred *sp = pred_subst((const TcPred *)p->cls->supers.items[i], &p->cls->params,
                                &p->args);
        TcEv *sev = ev_new(EV_SUPER);
        sev->of = ev;
        sev->cls = p->cls;
        sev->index = (int)i;
        add_given(tc, g, sp, sev, depth + 1);
    }
}

void push_given_evs(Tc *tc, const Vec *preds, const Vec *evs)
{
    Givens *g = (Givens *)xcalloc(1, sizeof *g);
    size_t i;
    g->up = tc->givens;
    for (i = 0; i < preds->len; i++) {
        add_given(tc, g, (TcPred *)preds->items[i], (TcEv *)evs->items[i], 0);
    }
    tc->givens = g;
}

void push_givens(Tc *tc, const Vec *preds, Vec *dicts)
{
    Vec evs;
    size_t i;
    vec_init(&evs);
    for (i = 0; i < preds->len; i++) {
        const TcPred *p = (const TcPred *)preds->items[i];
        TcEv *ev = ev_new(EV_VAR);
        if (p->cls != NULL) {
            ev->var = fresh_dict(tc);
            vec_push(dicts, (void *)ev->var);
        }
        vec_push(&evs, ev);
    }
    push_given_evs(tc, preds, &evs);
}

void pop_givens(Tc *tc)
{
    tc->givens = tc->givens->up;
}

/* --- Solving ------------------------------------------------------------------ */

static int is_fam_app(Tc *tc, Ty *t)
{
    return ty_whnf(tc, t)->kind == T_FAM;
}

/* Improve with the functional dependencies of the class: when the
 * arguments that determine the others match an instance or a given,
 * unify the determined ones. Returns 1 when it unified something. */
static int improve(Tc *tc, Wanted *w)
{
    TcClass *cls = w->pred->cls;
    size_t f;
    size_t i;
    size_t j;
    int progress = 0;
    for (f = 0; f < cls->fundeps.len; f++) {
        const TcFunDep *fd = (const TcFunDep *)cls->fundeps.items[f];
        Givens *g;
        /* Givens. */
        for (g = w->givens; g != NULL; g = g->up) {
            for (i = 0; i < g->preds.len; i++) {
                const TcPred *gp = (const TcPred *)g->preds.items[i];
                int same = 1;
                if (gp->cls != cls) {
                    continue;
                }
                for (j = 0; j < fd->from.len && same; j++) {
                    size_t k = (size_t)fd->from.items[j];
                    same = ty_equal(tc, (Ty *)gp->args.items[k], (Ty *)w->pred->args.items[k]);
                }
                if (!same) {
                    continue;
                }
                for (j = 0; j < fd->to.len; j++) {
                    size_t k = (size_t)fd->to.items[j];
                    if (!ty_equal(tc, (Ty *)gp->args.items[k], (Ty *)w->pred->args.items[k])) {
                        unify(tc, (Ty *)gp->args.items[k], (Ty *)w->pred->args.items[k]);
                        progress = 1;
                    }
                }
            }
        }
        /* Instances. */
        for (i = 0; i < cls->instances.len; i++) {
            const TcInst *inst = (const TcInst *)cls->instances.items[i];
            Vec to;
            int ok = 1;
            vec_init(&to);
            for (j = 0; j < inst->vars.len; j++) {
                vec_push(&to, NULL);
            }
            for (j = 0; j < fd->from.len && ok; j++) {
                size_t k = (size_t)fd->from.items[j];
                ok = ty_match(tc, (Ty *)inst->head.items[k], (Ty *)w->pred->args.items[k],
                              &inst->vars, &to);
            }
            if (!ok) {
                continue;
            }
            for (j = 0; j < to.len; j++) {
                if (to.items[j] == NULL) {
                    to.items[j] = ty_meta(tc);
                }
            }
            for (j = 0; j < fd->to.len; j++) {
                size_t k = (size_t)fd->to.items[j];
                Ty *want = ty_subst((Ty *)inst->head.items[k], &inst->vars, &to);
                if (!ty_equal(tc, want, (Ty *)w->pred->args.items[k])) {
                    unify(tc, want, (Ty *)w->pred->args.items[k]);
                    progress = 1;
                }
            }
            break;
        }
    }
    return progress;
}

static char *type_rep(Tc *tc, Ty *t)
{
    Vec metas;
    vec_init(&metas);
    ty_metas(tc, t, &metas);
    if (metas.len > 0) {
        return NULL;
    }
    return ty_str(zonk(tc, t));
}

/* 1: solved; 0: stuck. New wanteds go to out. */
static int solve_one(Tc *tc, Wanted *w, Vec *out)
{
    TcPred *p = w->pred;
    Givens *g;
    size_t i;
    size_t j;
    if (p->cls == NULL) {
        Ty *a = ty_whnf(tc, p->a);
        Ty *b = ty_whnf(tc, p->b);
        if (ty_equal(tc, a, b)) {
            return 1;
        }
        if (a->kind == T_META || b->kind == T_META || (!is_fam_app(tc, a) && !is_fam_app(tc, b))) {
            Pos saved = tc->pos;
            tc->pos = w->pos;
            unify(tc, a, b);
            tc->pos = saved;
            return 1;
        }
        for (g = w->givens; g != NULL; g = g->up) {
            for (i = 0; i < g->preds.len; i++) {
                const TcPred *gp = (const TcPred *)g->preds.items[i];
                if (gp->cls != NULL) {
                    continue;
                }
                if (ty_equal(tc, gp->a, a)) {
                    unify(tc, gp->b, b);
                    return 1;
                }
                if (ty_equal(tc, gp->b, a)) {
                    unify(tc, gp->a, b);
                    return 1;
                }
                if (ty_equal(tc, gp->a, b)) {
                    unify(tc, gp->b, a);
                    return 1;
                }
                if (ty_equal(tc, gp->b, b)) {
                    unify(tc, gp->a, a);
                    return 1;
                }
            }
        }
        return 0;
    }
    if (p->cls->fundeps.len > 0) {
        improve(tc, w);
    }
    for (g = w->givens; g != NULL; g = g->up) {
        for (i = 0; i < g->preds.len; i++) {
            if (pred_equal(tc, (const TcPred *)g->preds.items[i], p)) {
                w->ev->ref = (TcEv *)g->evs.items[i];
                return 1;
            }
        }
    }
    if (p->cls->typeable && p->args.len == 1) {
        char *rep = type_rep(tc, (Ty *)p->args.items[0]);
        if (rep != NULL) {
            TcEv *ev = ev_new(EV_TYPEREP);
            ev->cls = p->cls;
            ev->rep = rep;
            w->ev->ref = ev;
            return 1;
        }
        return 0;
    }
    for (i = 0; i < p->cls->instances.len; i++) {
        TcInst *inst = (TcInst *)p->cls->instances.items[i];
        Vec to;
        int ok = 1;
        TcEv *ev;
        vec_init(&to);
        for (j = 0; j < inst->vars.len; j++) {
            vec_push(&to, NULL);
        }
        for (j = 0; j < inst->head.len && ok; j++) {
            ok = ty_match(tc, (Ty *)inst->head.items[j], (Ty *)p->args.items[j], &inst->vars,
                          &to);
        }
        if (!ok) {
            continue;
        }
        for (j = 0; j < to.len; j++) {
            if (to.items[j] == NULL) {
                to.items[j] = ty_meta(tc);
            }
        }
        ev = ev_new(EV_INST);
        ev->inst = inst;
        for (j = 0; j < inst->ctx.len; j++) {
            TcPred *sub = pred_subst((const TcPred *)inst->ctx.items[j], &inst->vars, &to);
            Wanted *sw = (Wanted *)xcalloc(1, sizeof *sw);
            sw->pred = sub;
            sw->givens = w->givens;
            sw->pos = w->pos;
            sw->path = w->path;
            sw->level = w->level;
            if (sub->cls != NULL) {
                sw->ev = ev_new(EV_HOLE);
                vec_push(&ev->args, sw->ev);
            }
            vec_push(out, sw);
        }
        w->ev->ref = ev;
        return 1;
    }
    return 0;
}

void solve(Tc *tc)
{
    int progress = 1;
    while (progress) {
        Vec rest;
        Vec work = tc->wanted;
        size_t i;
        progress = 0;
        vec_init(&rest);
        vec_init(&tc->wanted);
        for (i = 0; i < work.len; i++) {
            Wanted *w = (Wanted *)work.items[i];
            Vec out;
            size_t j;
            vec_init(&out);
            if (solve_one(tc, w, &out)) {
                progress = 1;
                for (j = 0; j < out.len; j++) {
                    vec_push(&rest, out.items[j]);
                }
            } else {
                vec_push(&rest, w);
            }
        }
        /* Wanteds that unification made during the pass. */
        for (i = 0; i < tc->wanted.len; i++) {
            vec_push(&rest, tc->wanted.items[i]);
            progress = 1;
        }
        tc->wanted = rest;
    }
}

/* --- Defaulting ---------------------------------------------------------------- */

static int class_is_numeric(const TcClass *c, int depth)
{
    size_t i;
    if (depth > 10) {
        return 0;
    }
    if (str_eq(c->name, "Num") || str_eq(c->name, "Fractional")) {
        return 1;
    }
    for (i = 0; i < c->supers.len; i++) {
        const TcPred *sp = (const TcPred *)c->supers.items[i];
        if (sp->cls != NULL && class_is_numeric(sp->cls, depth + 1)) {
            return 1;
        }
    }
    return 0;
}

static int has_instance(Tc *tc, TcClass *cls, Ty *t)
{
    size_t i;
    Vec to;
    if (cls->typeable) {
        return 1;
    }
    for (i = 0; i < cls->instances.len; i++) {
        const TcInst *inst = (const TcInst *)cls->instances.items[i];
        size_t j;
        if (inst->head.len != 1) {
            continue;
        }
        vec_init(&to);
        for (j = 0; j < inst->vars.len; j++) {
            vec_push(&to, NULL);
        }
        if (ty_match(tc, (Ty *)inst->head.items[0], t, &inst->vars, &to)) {
            return 1;
        }
    }
    return 0;
}

int apply_defaults(Tc *tc, Vec *wanteds)
{
    Vec metas;
    size_t i;
    size_t j;
    int progress = 0;
    Ty *candidates[2];
    vec_init(&metas);
    for (i = 0; i < wanteds->len; i++) {
        const Wanted *w = (const Wanted *)wanteds->items[i];
        Ty *t;
        if (w->pred->cls == NULL || w->pred->args.len != 1) {
            continue;
        }
        t = ty_whnf(tc, (Ty *)w->pred->args.items[0]);
        if (t->kind == T_META) {
            for (j = 0; j < metas.len && metas.items[j] != t; j++) {
            }
            if (j == metas.len) {
                vec_push(&metas, t);
            }
        }
    }
    candidates[0] = tc_known_type(tc, "GHC.Num.Integer", "Integer");
    candidates[1] = tc_known_type(tc, "GHC.Types", "Double");
    for (i = 0; i < metas.len; i++) {
        Ty *m = (Ty *)metas.items[i];
        int numeric = 0;
        int c;
        if (prune(m) != m) {
            continue;
        }
        for (j = 0; j < wanteds->len; j++) {
            const Wanted *w = (const Wanted *)wanteds->items[j];
            if (w->pred->cls != NULL && w->pred->args.len == 1
                && ty_whnf(tc, (Ty *)w->pred->args.items[0]) == m
                && class_is_numeric(w->pred->cls, 0)) {
                numeric = 1;
            }
        }
        if (!numeric) {
            continue;
        }
        for (c = 0; c < 2; c++) {
            int ok = candidates[c] != NULL;
            for (j = 0; j < wanteds->len && ok; j++) {
                const Wanted *w = (const Wanted *)wanteds->items[j];
                if (w->pred->cls != NULL && w->pred->args.len == 1
                    && ty_whnf(tc, (Ty *)w->pred->args.items[0]) == m) {
                    ok = has_instance(tc, w->pred->cls, candidates[c]);
                }
            }
            if (ok) {
                m->ref = candidates[c];
                progress = 1;
                break;
            }
        }
    }
    return progress;
}

void fail_wanted(Tc *tc, const Wanted *w)
{
    TcPred *p = pred_zonk(tc, w->pred);
    tc->pos = w->pos;
    if (p->cls == NULL) {
        tc_fail(tc, str_printf("cannot match the type %s with %s", ty_str(p->a), ty_str(p->b)));
    }
    tc_fail(tc, str_printf("no instance for %s", pred_str(p)));
}

void solve_final(Tc *tc)
{
    solve(tc);
    while (tc->wanted.len > 0 && apply_defaults(tc, &tc->wanted)) {
        solve(tc);
    }
    if (tc->wanted.len > 0) {
        fail_wanted(tc, (const Wanted *)tc->wanted.items[0]);
    }
}
