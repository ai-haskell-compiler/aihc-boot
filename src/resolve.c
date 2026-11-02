/* The resolution of one module: a record for every identifier
 * occurrence.
 *
 * The records must agree with aihc-resolve, so the spans follow its rules:
 *
 * - A use of a name, and a variable that a pattern binds, has the span of
 *   the name as written.
 * - A name that a declaration defines starts at the start of the
 *   declaration, or after its keyword (`data `, `newtype `, `class `), and
 *   is as wide as the name without parentheses. A constructor starts at
 *   the start of its constructor declaration.
 * - A type operator in parentheses has the span of the parentheses.
 * - Built-in syntax, such as `()` or `[a]`, is a `syntax` record with the
 *   span of the whole form.
 * - Export lists, import lists, fixity declarations, type variables, the
 *   field names of a declaration, and `[]` and tuples in patterns have no
 *   records.
 *
 * The resolver refuses the constructs that it does not handle yet, with
 * the position of the construct, rather than give records that can be
 * wrong.
 *
 * Each function returns NULL on success, or the error.
 */
#include <stdlib.h>
#include <string.h>

#include "resolve.h"

/* A local term name in scope, with its binder id. */
typedef struct {
    const char *name;
    unsigned long id;
} Local;

typedef struct {
    Local *items;
    size_t len;
    size_t cap;
} LocalVec;

static void locals_push(LocalVec *v, const char *name, unsigned long id)
{
    if (v->len == v->cap) {
        v->cap = v->cap ? v->cap * 2 : 32;
        v->items = (Local *)xrealloc(v->items, v->cap * sizeof *v->items);
    }
    v->items[v->len].name = name;
    v->items[v->len].id = id;
    v->len++;
}

typedef struct {
    const Program *program;
    /* The top-level scope of the module. */
    const Scope *scope;
    /* The local term names in scope, innermost last. */
    LocalVec locals;
    unsigned long next_local;
    OccVec out;
} Resolver;

/* Count the characters (code points) of a UTF-8 string. */
static unsigned long char_count(const char *s)
{
    unsigned long n = 0;
    for (; *s != '\0'; s++) {
        if (((unsigned char)*s & 0xC0) != 0x80) {
            n++;
        }
    }
    return n;
}

/* The span of a name as wide as name, from start. */
static RecSpan narrow(Pos start, const char *name)
{
    RecSpan s;
    s.start_line = start.line;
    s.start_col = start.col;
    s.end_line = start.line;
    s.end_col = start.col + char_count(name);
    return s;
}

/* start moved to the right by the width of keyword. */
static Pos after(Pos start, const char *keyword)
{
    Pos p;
    p.line = start.line;
    p.col = start.col + char_count(keyword);
    return p;
}

static RecSpan span(Span s)
{
    RecSpan r;
    r.start_line = s.start.line;
    r.start_col = s.start.col;
    r.end_line = s.end.line;
    r.end_col = s.end.col;
    return r;
}

/* Whether a constructor name is built-in syntax: `()`, `[]` or a tuple. */
static int is_builtin_con(const char *name)
{
    return str_eq(name, "()") || str_eq(name, "[]") || str_starts_with(name, "(,");
}

static Target target_syntax(void)
{
    Target t;
    memset(&t, 0, sizeof t);
    t.kind = TARGET_SYNTAX;
    return t;
}

static Target target_local(unsigned long id)
{
    Target t;
    memset(&t, 0, sizeof t);
    t.kind = TARGET_LOCAL;
    t.id = id;
    return t;
}

static Target target_error(const char *message)
{
    Target t;
    memset(&t, 0, sizeof t);
    t.kind = TARGET_ERROR;
    t.message = message;
    return t;
}

static void emit(Resolver *r, RecSpan s, Namespace ns, Target target)
{
    Occurrence o;
    o.span = s;
    o.ns = ns;
    o.target = target;
    occvec_push(&r->out, o);
}

static Target lookup(const Resolver *r, Space space, const char *qual, const char *name)
{
    const Scope *scope;
    const Entity *found = NULL;
    if (space == SPACE_TERM && qual[0] == '\0') {
        size_t i = r->locals.len;
        while (i > 0) {
            i--;
            if (str_eq(r->locals.items[i].name, name)) {
                return target_local(r->locals.items[i].id);
            }
        }
    }
    scope = scope_qualifier(r->scope, qual);
    if (scope != NULL) {
        found = (const Entity *)strmap_get(space == SPACE_TERM ? &scope->terms
                                                               : &scope->types,
                                           name);
    }
    if (found != NULL) {
        return entity_target(found);
    }
    return target_error("unbound");
}

static Target lookup_q(const Resolver *r, Space space, const QName *name)
{
    return lookup(r, space, name->qual, name->name);
}

static Target lookup_top(const Resolver *r, Space space, const char *name)
{
    return lookup(r, space, "", name);
}

/* A new local binder for name, in scope until the caller truncates the
 * local scope. */
static unsigned long bind_local(Resolver *r, const char *name)
{
    unsigned long id = r->next_local++;
    locals_push(&r->locals, name, id);
    return id;
}

static ResolveError *ty(Resolver *r, const Type *t);
static ResolveError *context(Resolver *r, const Type *ctx);
static ResolveError *expr(Resolver *r, const Expr *e);
static ResolveError *rhs(Resolver *r, const Rhs *x);
static ResolveError *pats(Resolver *r, const Vec *ps);

/* --- Declarations -------------------------------------------------------- */

/* The kinds of the type variables of a declaration head (TyVarBind *). */
static ResolveError *binder_kinds(Resolver *r, const Vec *vars)
{
    size_t i;
    ResolveError *e;
    for (i = 0; i < vars->len; i++) {
        const TyVarBind *var = (const TyVarBind *)vars->items[i];
        if (var->kind != NULL && (e = ty(r, var->kind)) != NULL) {
            return e;
        }
    }
    return NULL;
}

/* The name and the argument patterns of a function binding. Returns 0 for
 * a binding that is not a function binding. */
static int fun_lhs(const Lhs *lhs, const char **name, Vec *args)
{
    vec_init(args);
    if (lhs->kind == LHS_FUN) {
        *name = lhs->name.name;
        *args = lhs->args;
        return 1;
    }
    if (lhs->kind == LHS_INFIX && lhs->args.len == 0) {
        *name = bare(lhs->name.name);
        vec_push(args, lhs->left);
        vec_push(args, lhs->right);
        return 1;
    }
    return 0;
}

/* A top-level binding, a class default or an instance method.
 *
 * The local binders of a file are numbered in one sequence, but as in
 * aihc-resolve, each class default and instance method (reset) numbers
 * its binders from zero, and the sequence continues after it. */
static ResolveError *top_bind(Resolver *r, Pos pos, const Lhs *lhs, const Rhs *x,
                              const Entity *class, int reset)
{
    const char *name;
    Vec ps;
    Target target;
    unsigned long saved;
    size_t mark;
    ResolveError *result;
    if (!fun_lhs(lhs, &name, &ps)) {
        return unsupported("this binding", &pos);
    }
    target = lookup_top(r, SPACE_TERM, name);
    if (target.kind == TARGET_ERROR && class != NULL) {
        /* An instance method of a class that is in scope only by its
         * class name. */
        const Vec *children = program_children(r->program, class);
        size_t i;
        for (i = 0; children != NULL && i < children->len; i++) {
            const Child *child = (const Child *)children->items[i];
            if (child->space == SPACE_TERM && str_eq(child->name, name)) {
                Entity method;
                method.package = class->package;
                method.module = class->module;
                method.name = name;
                target = entity_target(&method);
                break;
            }
        }
    }
    emit(r, narrow(pos, name), NS_TERM, target);
    saved = r->next_local;
    if (reset) {
        r->next_local = 0;
    }
    mark = r->locals.len;
    result = pats(r, &ps);
    if (result == NULL) {
        result = rhs(r, x);
    }
    r->locals.len = mark;
    if (reset) {
        r->next_local = saved;
    }
    return result;
}

/* A `data` or `newtype` declaration. */
static ResolveError *data_decl(Resolver *r, const Decl *decl)
{
    const char *keyword = decl->newtype ? "newtype " : "data ";
    ResolveError *e;
    size_t i;
    size_t j;
    emit(r, narrow(after(decl->pos, keyword), decl->name.name), NS_TYPE,
         lookup_top(r, SPACE_TYPE, decl->name.name));
    if ((e = binder_kinds(r, &decl->vars)) != NULL) {
        return e;
    }
    if (decl->kind_sig != NULL && (e = ty(r, decl->kind_sig)) != NULL) {
        return e;
    }
    for (i = 0; i < decl->cons.len; i++) {
        const ConDecl *con = (const ConDecl *)decl->cons.items[i];
        if (con->forall.len > 0 || con->ctx != NULL) {
            return unsupported("an existential constructor", &con->pos);
        }
        switch (con->kind) {
        case CON_PREFIX:
            if (str_eq(con->name.name, "[]")) {
                emit(r, span(con->name.span), NS_TERM, target_syntax());
            } else {
                emit(r, narrow(con->pos, con->name.name), NS_TERM,
                     lookup_top(r, SPACE_TERM, con->name.name));
            }
            for (j = 0; j < con->args.len; j++) {
                if ((e = ty(r, ((const BangType *)con->args.items[j])->ty)) != NULL) {
                    return e;
                }
            }
            break;
        case CON_INFIX: {
            const char *op = bare(con->name.name);
            emit(r, narrow(con->pos, op), NS_TERM, lookup_top(r, SPACE_TERM, op));
            if ((e = ty(r, con->left.ty)) != NULL) {
                return e;
            }
            if ((e = ty(r, con->right.ty)) != NULL) {
                return e;
            }
            break;
        }
        case CON_RECORD:
            emit(r, narrow(con->pos, con->name.name), NS_TERM,
                 lookup_top(r, SPACE_TERM, con->name.name));
            for (j = 0; j < con->fields.len; j++) {
                if ((e = ty(r, ((const Field *)con->fields.items[j])->ty.ty)) != NULL) {
                    return e;
                }
            }
            break;
        }
    }
    for (i = 0; i < decl->deriving.len; i++) {
        const Deriving *clause = (const Deriving *)decl->deriving.items[i];
        if (clause->via != NULL) {
            return unsupported("deriving via", &decl->pos);
        }
        for (j = 0; j < clause->classes.len; j++) {
            if ((e = ty(r, (const Type *)clause->classes.items[j])) != NULL) {
                return e;
            }
        }
    }
    return NULL;
}

/* The context and the head of an instance. Sets *class to the class, for
 * the method bindings, or to NULL. */
static ResolveError *instance_head(Resolver *r, const Type *head, Pos pos,
                                   Entity **class)
{
    const Type *c;
    ResolveError *e;
    *class = NULL;
    if (head->kind == TY_QUAL) {
        if ((e = context(r, head->a)) != NULL) {
            return e;
        }
        head = head->b;
    }
    if ((e = ty(r, head)) != NULL) {
        return e;
    }
    c = head;
    while (c->kind == TY_APP) {
        c = c->a;
    }
    if (c->kind == TY_CON) {
        Target t = lookup_q(r, SPACE_TYPE, &c->con);
        if (t.kind == TARGET_TOP) {
            Entity *entity = (Entity *)xmalloc(sizeof *entity);
            entity->package = t.package;
            entity->module = t.module;
            entity->name = t.name;
            *class = entity;
        }
        return NULL;
    }
    return unsupported("this instance head", &pos);
}

static ResolveError *top_decl(Resolver *r, const Decl *decl)
{
    ResolveError *e;
    size_t i;
    switch (decl->kind) {
    case DECL_TYPE_SIG:
        for (i = 0; i < decl->names.len; i++) {
            const char *name = ((const Ident *)decl->names.items[i])->name;
            emit(r, narrow(decl->pos, name), NS_TERM, lookup_top(r, SPACE_TERM, name));
        }
        return ty(r, decl->ty);
    case DECL_FIXITY:
        return NULL;
    case DECL_BIND:
        return top_bind(r, decl->pos, &decl->lhs, decl->rhs, NULL, 0);
    case DECL_DATA:
        return data_decl(r, decl);
    case DECL_TYPE_SYN:
        emit(r, narrow(after(decl->pos, "type "), decl->name.name), NS_TYPE,
             lookup_top(r, SPACE_TYPE, decl->name.name));
        if ((e = binder_kinds(r, &decl->vars)) != NULL) {
            return e;
        }
        return ty(r, decl->ty);
    case DECL_TYPE_FAMILY: {
        /* The record spans the head: the name and its variables. */
        Span head;
        head.start = decl->name.span.start;
        head.end = decl->name.span.end;
        for (i = 0; i < decl->vars.len; i++) {
            const TyVarBind *var = (const TyVarBind *)decl->vars.items[i];
            if (var->kind != NULL) {
                return unsupported("a kinded type family parameter", &decl->pos);
            }
            head.end = var->name.span.end;
        }
        emit(r, span(head), NS_TYPE, lookup_top(r, SPACE_TYPE, decl->name.name));
        if (decl->kind_sig != NULL) {
            return ty(r, decl->kind_sig);
        }
        return NULL;
    }
    case DECL_CLASS:
        emit(r, narrow(after(decl->pos, "class "), decl->name.name), NS_TYPE,
             lookup_top(r, SPACE_TYPE, decl->name.name));
        if (decl->ctx != NULL && (e = context(r, decl->ctx)) != NULL) {
            return e;
        }
        if ((e = binder_kinds(r, &decl->vars)) != NULL) {
            return e;
        }
        for (i = 0; i < decl->body.len; i++) {
            const Decl *item = (const Decl *)decl->body.items[i];
            switch (item->kind) {
            case DECL_TYPE_SIG:
            case DECL_TYPE_FAMILY:
                e = top_decl(r, item);
                break;
            case DECL_DEFAULT_SIG:
                e = ty(r, item->ty);
                break;
            case DECL_BIND:
                e = top_bind(r, item->pos, &item->lhs, item->rhs, NULL, 1);
                break;
            default:
                e = unsupported("this class item", &item->pos);
                break;
            }
            if (e != NULL) {
                return e;
            }
        }
        return NULL;
    case DECL_INSTANCE: {
        Entity *class;
        if ((e = instance_head(r, decl->ty, decl->pos, &class)) != NULL) {
            return e;
        }
        for (i = 0; i < decl->body.len; i++) {
            const Decl *item = (const Decl *)decl->body.items[i];
            switch (item->kind) {
            case DECL_BIND:
                e = top_bind(r, item->pos, &item->lhs, item->rhs, class, 1);
                break;
            case DECL_TYPE_SIG:
                e = ty(r, item->ty);
                break;
            default:
                e = unsupported("this instance item", &item->pos);
                break;
            }
            if (e != NULL) {
                return e;
            }
        }
        return NULL;
    }
    default:
        return unsupported("this declaration", &decl->pos);
    }
}

/* The binders of a group of local declarations, of a `where` or a `let`.
 * They are in scope for the whole group and its body; the caller
 * truncates the scope. Sets *group to where the group starts in the local
 * scope.
 *
 * As in aihc-resolve, every clause and every signature of one name gets a
 * new binder, and the last binder of a name wins. */
static ResolveError *bind_group(Resolver *r, const Vec *decls, size_t *group)
{
    size_t i;
    *group = r->locals.len;
    for (i = 0; i < decls->len; i++) {
        const Decl *decl = (const Decl *)decls->items[i];
        switch (decl->kind) {
        case DECL_BIND:
            if (decl->lhs.kind == LHS_FUN) {
                bind_local(r, decl->lhs.name.name);
            } else if (decl->lhs.kind == LHS_INFIX) {
                bind_local(r, bare(decl->lhs.name.name));
            } else {
                return unsupported("a local pattern binding", &decl->pos);
            }
            break;
        case DECL_TYPE_SIG:
            if (decl->names.len == 1) {
                bind_local(r, ((const Ident *)decl->names.items[0])->name);
            }
            break;
        case DECL_FIXITY:
            break;
        default:
            return unsupported("this local declaration", &decl->pos);
        }
    }
    return NULL;
}

/* The declarations of a local group that bind_group bound at group. */
static ResolveError *group_decls(Resolver *r, size_t group, const Vec *decls)
{
    size_t i;
    size_t j;
    ResolveError *e;
    for (i = 0; i < decls->len; i++) {
        const Decl *decl = (const Decl *)decls->items[i];
        if (decl->kind == DECL_TYPE_SIG) {
            for (j = 0; j < decl->names.len; j++) {
                /* A name that the group does not bind has no record. */
                const char *name = ((const Ident *)decl->names.items[j])->name;
                size_t k = r->locals.len;
                while (k > group) {
                    k--;
                    if (str_eq(r->locals.items[k].name, name)) {
                        emit(r, narrow(decl->pos, name), NS_TERM,
                             target_local(r->locals.items[k].id));
                        break;
                    }
                }
            }
            if ((e = ty(r, decl->ty)) != NULL) {
                return e;
            }
        } else if (decl->kind == DECL_BIND) {
            const char *name;
            Vec ps;
            size_t mark;
            if (!fun_lhs(&decl->lhs, &name, &ps)) {
                return unsupported("this binding", &decl->pos);
            }
            emit(r, narrow(decl->pos, name), NS_TERM, lookup_top(r, SPACE_TERM, name));
            mark = r->locals.len;
            e = pats(r, &ps);
            if (e == NULL) {
                e = rhs(r, decl->rhs);
            }
            r->locals.len = mark;
            if (e != NULL) {
                return e;
            }
        }
    }
    return NULL;
}

/* Guards: each one sees the binders of the ones before it (Stmt *). The
 * caller truncates the scope. */
static ResolveError *stmts(Resolver *r, const Vec *ss)
{
    size_t i;
    ResolveError *e;
    for (i = 0; i < ss->len; i++) {
        const Stmt *stmt = (const Stmt *)ss->items[i];
        switch (stmt->kind) {
        case STMT_EXPR:
            if ((e = expr(r, stmt->expr)) != NULL) {
                return e;
            }
            break;
        case STMT_BIND: {
            Vec one;
            if ((e = expr(r, stmt->expr)) != NULL) {
                return e;
            }
            vec_init(&one);
            vec_push(&one, stmt->pat);
            if ((e = pats(r, &one)) != NULL) {
                return e;
            }
            break;
        }
        case STMT_LET: {
            size_t group;
            if ((e = bind_group(r, &stmt->decls, &group)) != NULL) {
                return e;
            }
            if ((e = group_decls(r, group, &stmt->decls)) != NULL) {
                return e;
            }
            break;
        }
        }
    }
    return NULL;
}

static ResolveError *rhs_body(Resolver *r, const Rhs *x)
{
    size_t group;
    size_t i;
    ResolveError *e;
    if ((e = bind_group(r, &x->wheres, &group)) != NULL) {
        return e;
    }
    if (!x->guarded) {
        if ((e = expr(r, x->plain)) != NULL) {
            return e;
        }
    } else {
        for (i = 0; i < x->guards.len; i++) {
            const GuardedRhs *guarded = (const GuardedRhs *)x->guards.items[i];
            size_t inner = r->locals.len;
            e = stmts(r, &guarded->guards);
            if (e == NULL) {
                e = expr(r, guarded->body);
            }
            r->locals.len = inner;
            if (e != NULL) {
                return e;
            }
        }
    }
    return group_decls(r, group, &x->wheres);
}

/* A right-hand side. As in aihc-resolve, the body comes before the
 * `where` declarations, which matters for the numbering of binders. */
static ResolveError *rhs(Resolver *r, const Rhs *x)
{
    size_t mark = r->locals.len;
    ResolveError *e = rhs_body(r, x);
    r->locals.len = mark;
    return e;
}

/* --- Patterns ------------------------------------------------------------ */

static void pat_binder(Resolver *r, const Ident *name, LocalVec *bound)
{
    unsigned long id = r->next_local++;
    emit(r, span(name->span), NS_TERM, target_local(id));
    locals_push(bound, name->name, id);
}

static ResolveError *pat(Resolver *r, const Pat *p, LocalVec *bound)
{
    size_t i;
    ResolveError *e;
    switch (p->kind) {
    case PAT_VAR:
        pat_binder(r, &p->var, bound);
        return NULL;
    case PAT_WILDCARD:
        return NULL;
    case PAT_CON:
        if (!is_builtin_con(p->con.name)) {
            emit(r, span(p->con.span), NS_TERM, lookup_q(r, SPACE_TERM, &p->con));
        }
        for (i = 0; i < p->args.len; i++) {
            if ((e = pat(r, (const Pat *)p->args.items[i], bound)) != NULL) {
                return e;
            }
        }
        return NULL;
    case PAT_INFIX:
        if ((e = pat(r, p->a, bound)) != NULL) {
            return e;
        }
        for (i = 0; i < p->steps.len; i++) {
            const InfixStep *step = (const InfixStep *)p->steps.items[i];
            emit(r, span(step->op.span), NS_TERM, lookup_q(r, SPACE_TERM, &step->op));
            if ((e = pat(r, (const Pat *)step->operand, bound)) != NULL) {
                return e;
            }
        }
        return NULL;
    case PAT_TUPLE:
    case PAT_LIST:
        for (i = 0; i < p->args.len; i++) {
            if ((e = pat(r, (const Pat *)p->args.items[i], bound)) != NULL) {
                return e;
            }
        }
        return NULL;
    case PAT_PAREN:
    case PAT_LAZY:
    case PAT_BANG:
        return pat(r, p->a, bound);
    case PAT_AS:
        pat_binder(r, &p->var, bound);
        return pat(r, p->a, bound);
    case PAT_SIG:
        if ((e = pat(r, p->a, bound)) != NULL) {
            return e;
        }
        return ty(r, p->ty);
    default:
        return unsupported("this pattern", NULL);
    }
}

/* Bind patterns left to right (Pat *). The binders stay in scope; the
 * caller truncates the scope. */
static ResolveError *pats(Resolver *r, const Vec *ps)
{
    LocalVec bound;
    size_t i;
    ResolveError *e;
    memset(&bound, 0, sizeof bound);
    for (i = 0; i < ps->len; i++) {
        if ((e = pat(r, (const Pat *)ps->items[i], &bound)) != NULL) {
            return e;
        }
    }
    for (i = 0; i < bound.len; i++) {
        locals_push(&r->locals, bound.items[i].name, bound.items[i].id);
    }
    free(bound.items);
    return NULL;
}

/* --- Expressions --------------------------------------------------------- */

static void op(Resolver *r, const QName *name)
{
    emit(r, span(name->span), NS_TERM, lookup_q(r, SPACE_TERM, name));
}

static ResolveError *alts(Resolver *r, const Vec *as)
{
    size_t i;
    ResolveError *e;
    for (i = 0; i < as->len; i++) {
        const Alt *alt = (const Alt *)as->items[i];
        size_t mark = r->locals.len;
        Vec one;
        vec_init(&one);
        vec_push(&one, alt->pat);
        e = pats(r, &one);
        if (e == NULL) {
            e = rhs(r, alt->rhs);
        }
        r->locals.len = mark;
        if (e != NULL) {
            return e;
        }
    }
    return NULL;
}

static ResolveError *expr(Resolver *r, const Expr *x)
{
    size_t i;
    size_t mark;
    ResolveError *e;
    switch (x->kind) {
    case EXPR_VAR:
    case EXPR_CON:
        emit(r, span(x->name.span), NS_TERM,
             is_builtin_con(x->name.name) ? target_syntax()
                                          : lookup_q(r, SPACE_TERM, &x->name));
        return NULL;
    case EXPR_LIT:
        if (x->lit.kind == LIT_CHAR) {
            return NULL;
        }
        return unsupported("this expression", NULL);
    case EXPR_APP:
        if ((e = expr(r, x->a)) != NULL) {
            return e;
        }
        return expr(r, x->b);
    case EXPR_PAREN:
        return expr(r, x->a);
    case EXPR_SIG:
    case EXPR_TYPE_APP:
        if ((e = expr(r, x->a)) != NULL) {
            return e;
        }
        return ty(r, x->ty);
    case EXPR_INFIX:
        if ((e = expr(r, x->a)) != NULL) {
            return e;
        }
        for (i = 0; i < x->steps.len; i++) {
            const InfixStep *step = (const InfixStep *)x->steps.items[i];
            op(r, &step->op);
            if ((e = expr(r, (const Expr *)step->operand)) != NULL) {
                return e;
            }
        }
        return NULL;
    case EXPR_LEFT_SECTION:
        if ((e = expr(r, x->a)) != NULL) {
            return e;
        }
        op(r, &x->name);
        return NULL;
    case EXPR_RIGHT_SECTION:
        op(r, &x->name);
        return expr(r, x->a);
    case EXPR_LAMBDA:
        mark = r->locals.len;
        e = pats(r, &x->pats);
        if (e == NULL) {
            e = expr(r, x->a);
        }
        r->locals.len = mark;
        return e;
    case EXPR_LET: {
        size_t group;
        mark = r->locals.len;
        e = bind_group(r, &x->decls, &group);
        if (e == NULL) {
            e = group_decls(r, group, &x->decls);
        }
        if (e == NULL) {
            e = expr(r, x->a);
        }
        r->locals.len = mark;
        return e;
    }
    case EXPR_CASE:
        if ((e = expr(r, x->a)) != NULL) {
            return e;
        }
        return alts(r, &x->alts);
    case EXPR_LAMBDA_CASE:
        return alts(r, &x->alts);
    default:
        return unsupported("this expression", NULL);
    }
}

/* --- Types --------------------------------------------------------------- */

static ResolveError *ty(Resolver *r, const Type *t)
{
    ResolveError *e;
    switch (t->kind) {
    case TY_VAR:
    case TY_WILDCARD:
        return NULL;
    case TY_CON:
        if (str_eq(t->con.name, "()")) {
            emit(r, span(t->con.span), NS_TYPE, target_syntax());
            return NULL;
        }
        if (str_eq(t->con.name, "->") || str_eq(t->con.name, "[]")
            || str_starts_with(t->con.name, "(,")) {
            return unsupported("this built-in type constructor", &t->con.span.start);
        }
        emit(r, span(t->con.span), NS_TYPE, lookup_q(r, SPACE_TYPE, &t->con));
        return NULL;
    case TY_APP:
    case TY_FUN:
        if ((e = ty(r, t->a)) != NULL) {
            return e;
        }
        return ty(r, t->b);
    case TY_LIST:
        emit(r, span(t->span), NS_TYPE, target_syntax());
        return ty(r, t->a);
    case TY_PAREN:
        if (t->a->kind == TY_OP) {
            /* The operator of an infix type has the span of the
             * parentheses around it. */
            const Type *inner = t->a;
            emit(r, span(t->span), NS_TYPE, lookup_q(r, SPACE_TYPE, &inner->con));
            if ((e = ty(r, inner->a)) != NULL) {
                return e;
            }
            return ty(r, inner->b);
        }
        return ty(r, t->a);
    case TY_QUAL:
        if ((e = context(r, t->a)) != NULL) {
            return e;
        }
        return ty(r, t->b);
    case TY_KIND_SIG:
        if ((e = ty(r, t->a)) != NULL) {
            return e;
        }
        return ty(r, t->b);
    case TY_OP:
        return unsupported("a type operator without parentheses", &t->con.span.start);
    default:
        return unsupported("this type", NULL);
    }
}

/* A context: one constraint, or a tuple of constraints. The tuple is not
 * a tuple type, so it has no record. */
static ResolveError *context(Resolver *r, const Type *ctx)
{
    size_t i;
    ResolveError *e;
    if (ctx->kind == TY_TUPLE) {
        for (i = 0; i < ctx->items.len; i++) {
            if ((e = ty(r, (const Type *)ctx->items.items[i])) != NULL) {
                return e;
            }
        }
        return NULL;
    }
    return ty(r, ctx);
}

/* --- Entry point --------------------------------------------------------- */

ResolveError *resolve_module(const Program *p, const char *path, OccVec *out)
{
    const Module *module = program_module_at(p, path, NULL);
    ResolveError *e = NULL;
    Resolver r;
    size_t i;
    if (module == NULL) {
        return resolve_error(str_printf("resolve: no module at %s", path), NULL);
    }
    memset(&r, 0, sizeof r);
    r.program = p;
    r.scope = program_scope(p, module->name, &e);
    if (r.scope == NULL) {
        return e;
    }
    occvec_init(&r.out);
    for (i = 0; i < module->decls.len; i++) {
        if ((e = top_decl(&r, (const Decl *)module->decls.items[i])) != NULL) {
            return e;
        }
    }
    occvec_sort(&r.out);
    *out = r.out;
    return NULL;
}
