/* Derived instances: Eq, Ord, Show, Enum and Bounded.
 *
 * The code of a derived instance is written as Haskell source with
 * qualified names, parsed, and every qualified name is then made a
 * direct name (TC_DIRECT), so it does not depend on the scope of the
 * module. The instance then goes through the same checks as a written
 * one. Typeable needs no instance: the solver makes its evidence.
 */
#include <string.h>

#include "tc_int.h"

void tc_add_instance(Tc *tc, const Decl *decl, const Vec *body);

/* --- Direct names ------------------------------------------------------------ */

static void d_qname(QName *q)
{
    if (q->qual != NULL && q->qual[0] != '\0' && q->qual[0] != TC_DIRECT) {
        q->qual = str_printf("%c%s", TC_DIRECT, q->qual);
    }
}

static void d_expr(Expr *e);
static void d_pat(Pat *p);
static void d_decls(Vec *decls);

static void d_type(Type *t)
{
    size_t i;
    if (t == NULL) {
        return;
    }
    d_qname(&t->con);
    d_type(t->a);
    d_type(t->b);
    for (i = 0; i < t->items.len; i++) {
        d_type((Type *)t->items.items[i]);
    }
}

static void d_rhs(Rhs *rhs)
{
    size_t i;
    size_t j;
    if (rhs == NULL) {
        return;
    }
    d_expr(rhs->plain);
    for (i = 0; i < rhs->guards.len; i++) {
        GuardedRhs *g = (GuardedRhs *)rhs->guards.items[i];
        for (j = 0; j < g->guards.len; j++) {
            Stmt *s = (Stmt *)g->guards.items[j];
            d_expr(s->expr);
            d_pat(s->pat);
            d_decls(&s->decls);
        }
        d_expr(g->body);
    }
    d_decls(&rhs->wheres);
}

static void d_pat(Pat *p)
{
    size_t i;
    if (p == NULL) {
        return;
    }
    d_qname(&p->con);
    for (i = 0; i < p->args.len; i++) {
        d_pat((Pat *)p->args.items[i]);
    }
    d_pat(p->a);
    for (i = 0; i < p->steps.len; i++) {
        InfixStep *s = (InfixStep *)p->steps.items[i];
        d_qname(&s->op);
        d_pat((Pat *)s->operand);
    }
    for (i = 0; i < p->fields.len; i++) {
        FieldPat *f = (FieldPat *)p->fields.items[i];
        d_qname(&f->name);
        d_pat(f->pat);
    }
    d_expr(p->expr);
    d_type(p->ty);
}

static void d_expr(Expr *e)
{
    size_t i;
    if (e == NULL) {
        return;
    }
    d_qname(&e->name);
    d_expr(e->a);
    d_expr(e->b);
    d_expr(e->c);
    d_type(e->ty);
    for (i = 0; i < e->steps.len; i++) {
        InfixStep *s = (InfixStep *)e->steps.items[i];
        d_qname(&s->op);
        d_expr((Expr *)s->operand);
    }
    for (i = 0; i < e->pats.len; i++) {
        d_pat((Pat *)e->pats.items[i]);
    }
    for (i = 0; i < e->alts.len; i++) {
        Alt *a = (Alt *)e->alts.items[i];
        d_pat(a->pat);
        d_rhs(a->rhs);
    }
    d_decls(&e->decls);
    for (i = 0; i < e->stmts.len; i++) {
        Stmt *s = (Stmt *)e->stmts.items[i];
        d_expr(s->expr);
        d_pat(s->pat);
        d_decls(&s->decls);
    }
    for (i = 0; i < e->items.len; i++) {
        d_expr((Expr *)e->items.items[i]);
    }
    for (i = 0; i < e->fields.len; i++) {
        FieldUpdate *f = (FieldUpdate *)e->fields.items[i];
        d_qname(&f->name);
        d_expr(f->value);
    }
}

static void d_decls(Vec *decls)
{
    size_t i;
    size_t j;
    for (i = 0; i < decls->len; i++) {
        Decl *d = (Decl *)decls->items[i];
        d_type(d->ty);
        for (j = 0; j < d->lhs.args.len; j++) {
            d_pat((Pat *)d->lhs.args.items[j]);
        }
        d_pat(d->lhs.left);
        d_pat(d->lhs.right);
        d_pat(d->lhs.pat);
        d_rhs(d->rhs);
        d_decls(&d->body);
    }
}

/* --- Generation ---------------------------------------------------------------- */

typedef struct {
    Tc *tc;
    const Decl *data;
    TcTyCon *tycon;
    StrBuf b;
} Gen;

/* A constructor name as an expression or a pattern head, qualified. */
static void put_con(Gen *g, const TcCon *c)
{
    if (c->name[0] == ':') {
        sb_printf(&g->b, "(%s.%s)", c->module, c->name);
    } else {
        sb_printf(&g->b, "%s.%s", c->module, c->name);
    }
}

/* A pattern for constructor c with variables PREFIX1 .. PREFIXn. */
static void put_con_pat(Gen *g, const TcCon *c, const char *prefix)
{
    size_t i;
    sb_putc(&g->b, '(');
    if (c->name[0] == ':' && c->fields.len == 2) {
        sb_printf(&g->b, "%s1 %s.%s %s2)", prefix, c->module, c->name, prefix);
        return;
    }
    put_con(g, c);
    for (i = 0; i < c->fields.len; i++) {
        sb_printf(&g->b, " %s%lu", prefix, (unsigned long)i + 1);
    }
    sb_putc(&g->b, ')');
}

/* The type parameters that occur in field types other than as the head
 * of an application. */
static void arg_vars(Tc *tc, Ty *t, int head, Vec *out)
{
    size_t i;
    t = prune(t);
    switch (t->kind) {
    case T_BOUND:
        if (!head) {
            for (i = 0; i < out->len; i++) {
                if (out->items[i] == t) {
                    return;
                }
            }
            vec_push(out, t);
        }
        return;
    case T_APP:
        arg_vars(tc, t->fun, 1, out);
        arg_vars(tc, t->arg, 0, out);
        return;
    case T_FAM:
        for (i = 0; i < t->args.len; i++) {
            arg_vars(tc, (Ty *)t->args.items[i], 0, out);
        }
        return;
    default:
        return;
    }
}

static void put_head(Gen *g, const TcClass *cls)
{
    Vec ctx;
    size_t i;
    size_t j;
    vec_init(&ctx);
    for (i = 0; i < g->tycon->cons.len; i++) {
        const TcCon *c = (const TcCon *)g->tycon->cons.items[i];
        for (j = 0; j < c->fields.len; j++) {
            arg_vars(g->tc, (Ty *)c->fields.items[j], 0, &ctx);
        }
    }
    sb_puts(&g->b, "instance ");
    if (ctx.len > 0) {
        sb_putc(&g->b, '(');
        for (i = 0; i < ctx.len; i++) {
            if (i > 0) {
                sb_puts(&g->b, ", ");
            }
            sb_printf(&g->b, "%s.%s %s", cls->module, cls->name, ((Ty *)ctx.items[i])->name);
        }
        sb_puts(&g->b, ") => ");
    }
    sb_printf(&g->b, "%s.%s (%s.%s", cls->module, cls->name, g->tycon->module, g->tycon->name);
    for (i = 0; i < g->tycon->params.len; i++) {
        sb_printf(&g->b, " %s", ((Ty *)g->tycon->params.items[i])->name);
    }
    sb_puts(&g->b, ") where\n");
}

static void gen_eq(Gen *g)
{
    size_t i;
    size_t j;
    const Vec *cons = &g->tycon->cons;
    for (i = 0; i < cons->len; i++) {
        const TcCon *c = (const TcCon *)cons->items[i];
        sb_puts(&g->b, "  (==) ");
        put_con_pat(g, c, "x");
        sb_putc(&g->b, ' ');
        put_con_pat(g, c, "y");
        sb_puts(&g->b, " = ");
        for (j = 0; j < c->fields.len; j++) {
            sb_printf(&g->b, "(x%lu GHC.Classes.== y%lu) GHC.Classes.&& (", (unsigned long)j + 1,
                      (unsigned long)j + 1);
        }
        sb_puts(&g->b, "GHC.Types.True");
        for (j = 0; j < c->fields.len; j++) {
            sb_putc(&g->b, ')');
        }
        sb_putc(&g->b, '\n');
    }
    if (cons->len != 1) {
        sb_printf(&g->b, "  (==) _ _ = GHC.Types.%s\n", cons->len == 0 ? "True" : "False");
    }
}

static void gen_ord(Gen *g)
{
    size_t i;
    size_t j;
    const Vec *cons = &g->tycon->cons;
    if (cons->len == 0) {
        sb_puts(&g->b, "  compare _ _ = GHC.Types.EQ\n");
        return;
    }
    for (i = 0; i < cons->len; i++) {
        const TcCon *c = (const TcCon *)cons->items[i];
        sb_puts(&g->b, "  compare ");
        put_con_pat(g, c, "x");
        sb_putc(&g->b, ' ');
        put_con_pat(g, c, "y");
        sb_puts(&g->b, " = ");
        for (j = 0; j < c->fields.len; j++) {
            sb_printf(&g->b,
                      "case GHC.Classes.compare x%lu y%lu of { GHC.Types.LT -> GHC.Types.LT; "
                      "GHC.Types.GT -> GHC.Types.GT; GHC.Types.EQ -> ",
                      (unsigned long)j + 1, (unsigned long)j + 1);
        }
        sb_puts(&g->b, "GHC.Types.EQ");
        for (j = 0; j < c->fields.len; j++) {
            sb_puts(&g->b, " }");
        }
        sb_putc(&g->b, '\n');
    }
    if (cons->len > 1) {
        sb_puts(&g->b, "  compare a b = GHC.Classes.compare (tag a) (tag b)\n    where\n");
        for (i = 0; i < cons->len; i++) {
            const TcCon *c = (const TcCon *)cons->items[i];
            sb_puts(&g->b, "      tag ");
            sb_putc(&g->b, '(');
            put_con(g, c);
            for (j = 0; j < c->fields.len; j++) {
                sb_puts(&g->b, " _");
            }
            sb_printf(&g->b, ") = (%lu :: GHC.Types.Int)\n", (unsigned long)i);
        }
    }
}

static void put_string_lit(Gen *g, const char *s)
{
    sb_puts(&g->b, string_literal(s, strlen(s)));
}

static int con_fixity(Gen *g, const TcCon *c)
{
    const Fixity *f = (const Fixity *)strmap_get(&g->tc->fixities, tkey(c->module, c->name));
    return f != NULL ? f->prec : 9;
}

static void gen_show(Gen *g)
{
    size_t i;
    size_t j;
    const Vec *cons = &g->tycon->cons;
    for (i = 0; i < cons->len; i++) {
        const TcCon *c = (const TcCon *)cons->items[i];
        int record = c->fields.len > 0 && c->field_names.items[0] != NULL;
        sb_puts(&g->b, "  showsPrec d ");
        put_con_pat(g, c, "x");
        sb_puts(&g->b, " = ");
        if (c->fields.len == 0) {
            sb_puts(&g->b, "GHC.Show.showString ");
            put_string_lit(g, c->name[0] == ':' ? str_printf("(%s)", c->name) : c->name);
            sb_putc(&g->b, '\n');
            continue;
        }
        if (c->name[0] == ':' && c->fields.len == 2 && !record) {
            int p = con_fixity(g, c);
            sb_printf(&g->b,
                      "GHC.Show.showParen (d GHC.Classes.> %d) (\\s -> GHC.Show.showsPrec %d x1 "
                      "(GHC.Show.showString ",
                      p, p + 1);
            put_string_lit(g, str_printf(" %s ", c->name));
            sb_printf(&g->b, " (GHC.Show.showsPrec %d x2 s)))\n", p + 1);
            continue;
        }
        sb_printf(&g->b, "GHC.Show.showParen (d GHC.Classes.>= %d) (\\s -> GHC.Show.showString ",
                  record ? 11 : 11);
        if (record) {
            put_string_lit(g, str_printf("%s {", c->name[0] == ':' ? str_printf("(%s)", c->name)
                                                                    : c->name));
            for (j = 0; j < c->fields.len; j++) {
                const char *f = (const char *)c->field_names.items[j];
                sb_puts(&g->b, " (GHC.Show.showString ");
                put_string_lit(g, str_printf("%s%s = ", j > 0 ? ", " : "", f));
                sb_printf(&g->b, " (GHC.Show.showsPrec 0 x%lu", (unsigned long)j + 1);
            }
            sb_puts(&g->b, " (GHC.Show.showChar '}' s)");
            for (j = 0; j < c->fields.len; j++) {
                sb_puts(&g->b, "))");
            }
            sb_puts(&g->b, ")\n");
            continue;
        }
        put_string_lit(g, str_printf("%s ", c->name[0] == ':' ? str_printf("(%s)", c->name)
                                                              : c->name));
        for (j = 0; j < c->fields.len; j++) {
            if (j > 0) {
                sb_puts(&g->b, " (GHC.Show.showChar ' '");
            }
            sb_printf(&g->b, " (GHC.Show.showsPrec 11 x%lu", (unsigned long)j + 1);
        }
        sb_puts(&g->b, " s");
        for (j = 0; j < c->fields.len; j++) {
            sb_puts(&g->b, j > 0 ? "))" : ")");
        }
        sb_puts(&g->b, ")\n");
    }
    if (cons->len == 0) {
        sb_puts(&g->b, "  showsPrec _ _ = GHC.Err.error \"Void showsPrec\"\n");
    }
}

static int is_enumeration(const TcTyCon *t)
{
    size_t i;
    for (i = 0; i < t->cons.len; i++) {
        if (((const TcCon *)t->cons.items[i])->fields.len > 0) {
            return 0;
        }
    }
    return t->cons.len > 0;
}

static void gen_enum(Gen *g)
{
    size_t i;
    const Vec *cons = &g->tycon->cons;
    const TcCon *first;
    const TcCon *last;
    if (!is_enumeration(g->tycon)) {
        g->tc->pos = g->data->pos;
        tc_fail(g->tc, xstrdup("deriving Enum needs an enumeration type"));
    }
    first = (const TcCon *)cons->items[0];
    last = (const TcCon *)cons->items[cons->len - 1];
    for (i = 0; i < cons->len; i++) {
        sb_puts(&g->b, "  fromEnum ");
        put_con(g, (const TcCon *)cons->items[i]);
        sb_printf(&g->b, " = %lu\n", (unsigned long)i);
    }
    for (i = 0; i < cons->len; i++) {
        sb_printf(&g->b, "  toEnum %lu = ", (unsigned long)i);
        put_con(g, (const TcCon *)cons->items[i]);
        sb_putc(&g->b, '\n');
    }
    sb_puts(&g->b, "  toEnum _ = GHC.Err.error \"toEnum: bad argument\"\n");
    sb_puts(&g->b, "  enumFrom x = GHC.Enum.enumFromTo x ");
    put_con(g, last);
    sb_puts(&g->b, "\n  enumFromThen x y = GHC.Enum.enumFromThenTo x y (if GHC.Enum.fromEnum y "
                   "GHC.Classes.>= GHC.Enum.fromEnum x then ");
    put_con(g, last);
    sb_puts(&g->b, " else ");
    put_con(g, first);
    sb_puts(&g->b, ")\n");
}

static void gen_bounded(Gen *g)
{
    const Vec *cons = &g->tycon->cons;
    size_t j;
    if (is_enumeration(g->tycon)) {
        sb_puts(&g->b, "  minBound = ");
        put_con(g, (const TcCon *)cons->items[0]);
        sb_puts(&g->b, "\n  maxBound = ");
        put_con(g, (const TcCon *)cons->items[cons->len - 1]);
        sb_putc(&g->b, '\n');
        return;
    }
    if (cons->len != 1) {
        g->tc->pos = g->data->pos;
        tc_fail(g->tc, xstrdup("deriving Bounded needs an enumeration or one constructor"));
    }
    sb_puts(&g->b, "  minBound = ");
    put_con(g, (const TcCon *)cons->items[0]);
    for (j = 0; j < ((const TcCon *)cons->items[0])->fields.len; j++) {
        sb_puts(&g->b, " GHC.Enum.minBound");
    }
    sb_puts(&g->b, "\n  maxBound = ");
    put_con(g, (const TcCon *)cons->items[0]);
    for (j = 0; j < ((const TcCon *)cons->items[0])->fields.len; j++) {
        sb_puts(&g->b, " GHC.Enum.maxBound");
    }
    sb_putc(&g->b, '\n');
}

void tc_derive(Tc *tc, const Decl *data, TcTyCon *tycon, const Type *cls_type)
{
    Gen g;
    const Type *t = cls_type;
    const Entity *e;
    TcClass *cls;
    Module *m;
    SyntaxError err;
    Decl *inst;
    while (t->kind == TY_PAREN) {
        t = t->a;
    }
    if (t->kind != TY_CON) {
        tc_fail(tc, xstrdup("this deriving clause is not supported yet"));
    }
    e = tc_entity(tc, &t->con, SPACE_TYPE);
    if (e == NULL) {
        tc_fail(tc, str_printf("unbound class %s", t->con.name));
    }
    cls = tc_known_class(tc, e->module, e->name);
    if (cls == NULL) {
        tc_fail(tc, str_printf("%s is not a class", t->con.name));
    }
    if (cls->typeable) {
        return;
    }
    g.tc = tc;
    g.data = data;
    g.tycon = tycon;
    sb_init(&g.b);
    sb_puts(&g.b, "module Derived where\n");
    put_head(&g, cls);
    if (str_eq(cls->module, "GHC.Classes") && str_eq(cls->name, "Eq")) {
        gen_eq(&g);
    } else if (str_eq(cls->module, "GHC.Classes") && str_eq(cls->name, "Ord")) {
        gen_ord(&g);
    } else if (str_eq(cls->module, "GHC.Show") && str_eq(cls->name, "Show")) {
        gen_show(&g);
    } else if (str_eq(cls->module, "GHC.Enum") && str_eq(cls->name, "Enum")) {
        gen_enum(&g);
    } else if (str_eq(cls->module, "GHC.Enum") && str_eq(cls->name, "Bounded")) {
        gen_bounded(&g);
    } else {
        tc_fail(tc, str_printf("deriving %s is not supported yet", cls->name));
    }
    m = parse_source(g.b.s, g.b.len, &err);
    if (m == NULL) {
        tc_fail(tc, str_printf("internal error: a derived instance does not parse: %s: %s\n%s",
                               err.message, "", g.b.s));
    }
    inst = (Decl *)m->decls.items[0];
    d_type(inst->ty);
    d_decls(&inst->body);
    inst->pos = data->pos;
    tc_add_instance(tc, inst, &inst->body);
}
