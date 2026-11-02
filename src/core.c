/* Core constructors and the Core printer. See core.h. */
#include <string.h>

#include "core.h"
#include "syntax.h"

Core *core_new(CoreKind kind)
{
    Core *e = (Core *)xcalloc(1, sizeof *e);
    e->kind = kind;
    vec_init(&e->args);
    vec_init(&e->binds);
    vec_init(&e->names);
    vec_init(&e->alts);
    return e;
}

Core *core_var(unsigned long up, unsigned long slot, const char *name)
{
    Core *e = core_new(CORE_VAR);
    e->up = up;
    e->slot = slot;
    e->name = name;
    return e;
}

Core *core_global(Global *g)
{
    Core *e = core_new(CORE_GLOBAL);
    e->global = g;
    return e;
}

Core *core_int(long value)
{
    Core *e = core_new(CORE_INT);
    e->ival = value;
    return e;
}

Core *core_char(unsigned long value)
{
    Core *e = core_new(CORE_CHAR);
    e->cval = value;
    return e;
}

Core *core_str(const char *s, size_t len)
{
    Core *e = core_new(CORE_STR);
    e->sval = s;
    e->slen = len;
    return e;
}

Core *core_app(Core *fun, const Vec *args)
{
    Core *e;
    size_t i;
    if (args->len == 0) {
        return fun;
    }
    /* Flatten `(f a) b` into `f a b`. */
    if (fun->kind == CORE_APP) {
        for (i = 0; i < args->len; i++) {
            vec_push(&fun->args, args->items[i]);
        }
        return fun;
    }
    e = core_new(CORE_APP);
    e->fun = fun;
    for (i = 0; i < args->len; i++) {
        vec_push(&e->args, args->items[i]);
    }
    return e;
}

Core *core_app1(Core *fun, Core *arg)
{
    Vec args;
    vec_init(&args);
    vec_push(&args, arg);
    return core_app(fun, &args);
}

Core *core_app2(Core *fun, Core *a, Core *b)
{
    Vec args;
    vec_init(&args);
    vec_push(&args, a);
    vec_push(&args, b);
    return core_app(fun, &args);
}

Core *core_lam(unsigned long arity, Core *body)
{
    Core *e = core_new(CORE_LAM);
    e->arity = arity;
    e->body = body;
    return e;
}

Core *core_let(const Vec *binds, Core *body)
{
    Core *e;
    size_t i;
    if (binds->len == 0) {
        return body;
    }
    e = core_new(CORE_LET);
    for (i = 0; i < binds->len; i++) {
        vec_push(&e->binds, binds->items[i]);
    }
    e->body = body;
    return e;
}

Core *core_case(Core *scrut, const Vec *alts)
{
    Core *e = core_new(CORE_CASE);
    size_t i;
    e->scrut = scrut;
    for (i = 0; i < alts->len; i++) {
        vec_push(&e->alts, alts->items[i]);
    }
    return e;
}

Core *core_con(ConInfo *con, const Vec *args)
{
    Core *e = core_new(CORE_CON);
    size_t i;
    e->con = con;
    for (i = 0; i < args->len; i++) {
        vec_push(&e->args, args->items[i]);
    }
    return e;
}

CoreAlt *alt_new(AltKind kind, Core *body)
{
    CoreAlt *a = (CoreAlt *)xcalloc(1, sizeof *a);
    a->kind = kind;
    a->body = body;
    vec_init(&a->names);
    return a;
}

/* --- Printer ------------------------------------------------------------ */

static void pr(StrBuf *out, const Core *e, int indent);

static int is_symbolic(const char *name)
{
    return name[0] != '\0' && !(name[0] == '_' || name[0] == '\''
                                || (name[0] >= 'a' && name[0] <= 'z')
                                || (name[0] >= 'A' && name[0] <= 'Z')
                                || name[0] == '(' || name[0] == '[');
}

static void pr_name(StrBuf *out, const char *name)
{
    if (is_symbolic(name)) {
        sb_putc(out, '(');
        sb_puts(out, name);
        sb_putc(out, ')');
    } else {
        sb_puts(out, name);
    }
}

static void pr_global_name(StrBuf *out, const Global *g)
{
    if (!str_eq(g->module, "Main")) {
        sb_puts(out, g->module);
        sb_putc(out, '.');
    }
    pr_name(out, g->name);
}

static void newline(StrBuf *out, int indent)
{
    int i;
    sb_putc(out, '\n');
    for (i = 0; i < indent; i++) {
        sb_puts(out, "  ");
    }
}

/* Whether e prints as one token, so it needs no parentheses as an
 * argument. */
static int atomic(const Core *e)
{
    switch (e->kind) {
    case CORE_VAR:
    case CORE_GLOBAL:
    case CORE_INT:
    case CORE_CHAR:
    case CORE_STR:
        return 1;
    case CORE_CON:
        return e->args.len == 0;
    default:
        return 0;
    }
}

static void pr_arg(StrBuf *out, const Core *e, int indent)
{
    if (atomic(e)) {
        pr(out, e, indent);
    } else {
        sb_putc(out, '(');
        pr(out, e, indent + 1);
        sb_putc(out, ')');
    }
}

static void pr_args(StrBuf *out, const Vec *args, int indent)
{
    size_t i;
    for (i = 0; i < args->len; i++) {
        sb_putc(out, ' ');
        pr_arg(out, (const Core *)args->items[i], indent);
    }
}

static void pr_binder_names(StrBuf *out, const Vec *names, unsigned long n)
{
    unsigned long i;
    for (i = 0; i < n; i++) {
        sb_putc(out, ' ');
        if (i < names->len) {
            pr_name(out, (const char *)names->items[i]);
        } else {
            sb_putc(out, '_');
        }
    }
}

static void pr_alt(StrBuf *out, const CoreAlt *a, int indent)
{
    switch (a->kind) {
    case ALT_CON:
        pr_name(out, a->con->name);
        pr_binder_names(out, &a->names, (unsigned long)a->con->arity);
        break;
    case ALT_INT:
        sb_putl(out, a->ival);
        break;
    case ALT_CHAR:
        sb_puts(out, char_literal(a->cval));
        break;
    case ALT_DEFAULT:
        sb_putc(out, '_');
        break;
    }
    sb_puts(out, " -> ");
    pr(out, a->body, indent + 1);
}

static void pr(StrBuf *out, const Core *e, int indent)
{
    size_t i;
    switch (e->kind) {
    case CORE_VAR:
        pr_name(out, e->name);
        break;
    case CORE_GLOBAL:
        pr_global_name(out, e->global);
        break;
    case CORE_INT:
        sb_putl(out, e->ival);
        break;
    case CORE_CHAR:
        sb_puts(out, char_literal(e->cval));
        break;
    case CORE_STR:
        sb_puts(out, string_literal(e->sval, e->slen));
        break;
    case CORE_APP:
        pr_arg(out, e->fun, indent);
        pr_args(out, &e->args, indent);
        break;
    case CORE_LAM:
        sb_putc(out, '\\');
        pr_binder_names(out, &e->names, e->arity);
        sb_puts(out, " ->");
        newline(out, indent + 1);
        pr(out, e->body, indent + 1);
        break;
    case CORE_LET:
        sb_puts(out, "let {");
        for (i = 0; i < e->binds.len; i++) {
            newline(out, indent + 1);
            if (i < e->names.len) {
                pr_name(out, (const char *)e->names.items[i]);
            } else {
                sb_putc(out, '_');
            }
            sb_puts(out, " = ");
            pr(out, (const Core *)e->binds.items[i], indent + 2);
            sb_putc(out, ';');
        }
        newline(out, indent);
        sb_puts(out, "} in");
        newline(out, indent);
        pr(out, e->body, indent);
        break;
    case CORE_CASE:
        sb_puts(out, "case ");
        pr(out, e->scrut, indent + 1);
        sb_puts(out, " of {");
        for (i = 0; i < e->alts.len; i++) {
            newline(out, indent + 1);
            pr_alt(out, (const CoreAlt *)e->alts.items[i], indent + 1);
            sb_putc(out, ';');
        }
        newline(out, indent);
        sb_putc(out, '}');
        break;
    case CORE_CON:
        pr_name(out, e->con->name);
        pr_args(out, &e->args, indent);
        break;
    }
}

void core_print(StrBuf *out, const Core *e)
{
    pr(out, e, 0);
}

void core_print_global(StrBuf *out, const Global *g)
{
    pr_global_name(out, g);
    sb_puts(out, " = ");
    if (g->body != NULL) {
        pr(out, g->body, 1);
    } else if (g->prim != NULL) {
        sb_puts(out, "<primitive>");
    } else {
        sb_puts(out, "<undefined>");
    }
    sb_putc(out, '\n');
}
