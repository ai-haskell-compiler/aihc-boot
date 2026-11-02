/* The records that `aihc-boot dump --stage resolve` and
 * `tools/resolve-oracle` print, one per line, tab separated:
 *
 *     module FILE ok
 *     module FILE error MESSAGE
 *     name FILE L1 C1 L2 C2 NS top PACKAGE MODULE NAME
 *     name FILE L1 C1 L2 C2 NS local ID
 *     name FILE L1 C1 L2 C2 NS syntax
 *     name FILE L1 C1 L2 C2 NS error MESSAGE
 *
 * NS is `term`, `type` or `module`. `L1 C1` is the start and `L2 C2` the
 * end of the identifier, 1-based, the end exclusive.
 */
#include <stdlib.h>
#include <string.h>

#include "resolve.h"

/* --- Spans, namespaces and targets -------------------------------------- */

static int ul_cmp(unsigned long a, unsigned long b)
{
    return a < b ? -1 : a > b ? 1 : 0;
}

int recspan_cmp(RecSpan a, RecSpan b)
{
    int c = ul_cmp(a.start_line, b.start_line);
    if (c == 0) {
        c = ul_cmp(a.start_col, b.start_col);
    }
    if (c == 0) {
        c = ul_cmp(a.end_line, b.end_line);
    }
    if (c == 0) {
        c = ul_cmp(a.end_col, b.end_col);
    }
    return c;
}

char *recspan_str(RecSpan s)
{
    return str_printf("%lu:%lu-%lu:%lu", s.start_line, s.start_col,
                      s.end_line, s.end_col);
}

const char *namespace_str(Namespace ns)
{
    switch (ns) {
    case NS_TERM:
        return "term";
    case NS_TYPE:
        return "type";
    default:
        return "module";
    }
}

int target_eq(const Target *a, const Target *b)
{
    if (a->kind != b->kind) {
        return 0;
    }
    switch (a->kind) {
    case TARGET_TOP:
        return str_eq(a->package, b->package) && str_eq(a->module, b->module)
               && str_eq(a->name, b->name);
    case TARGET_LOCAL:
        return a->id == b->id;
    case TARGET_SYNTAX:
        return 1;
    default:
        return str_eq(a->message, b->message);
    }
}

void target_write(StrBuf *b, const Target *t)
{
    switch (t->kind) {
    case TARGET_TOP:
        sb_printf(b, "top\t%s\t%s\t%s", t->package, t->module, t->name);
        break;
    case TARGET_LOCAL:
        sb_printf(b, "local\t%lu", t->id);
        break;
    case TARGET_SYNTAX:
        sb_puts(b, "syntax");
        break;
    default:
        sb_printf(b, "error\t%s", t->message);
        break;
    }
}

/* --- Occurrence arrays -------------------------------------------------- */

void occvec_init(OccVec *v)
{
    v->items = NULL;
    v->len = 0;
    v->cap = 0;
}

void occvec_push(OccVec *v, Occurrence o)
{
    if (v->len == v->cap) {
        v->cap = v->cap ? v->cap * 2 : 16;
        v->items = (Occurrence *)xrealloc(v->items, v->cap * sizeof *v->items);
    }
    v->items[v->len++] = o;
}

/* A merge sort of a[0..n) by span, with tmp as scratch space. It keeps
 * the order of equal spans. */
static void merge_sort(Occurrence *a, Occurrence *tmp, size_t n)
{
    size_t mid;
    size_t i;
    size_t j;
    size_t k;
    if (n < 2) {
        return;
    }
    mid = n / 2;
    merge_sort(a, tmp, mid);
    merge_sort(a + mid, tmp, n - mid);
    i = 0;
    j = mid;
    k = 0;
    while (i < mid && j < n) {
        if (recspan_cmp(a[j].span, a[i].span) < 0) {
            tmp[k++] = a[j++];
        } else {
            tmp[k++] = a[i++];
        }
    }
    while (i < mid) {
        tmp[k++] = a[i++];
    }
    while (j < n) {
        tmp[k++] = a[j++];
    }
    memcpy(a, tmp, n * sizeof *a);
}

void occvec_sort(OccVec *v)
{
    Occurrence *tmp;
    if (v->len < 2) {
        return;
    }
    tmp = (Occurrence *)xmalloc(v->len * sizeof *tmp);
    merge_sort(v->items, tmp, v->len);
    free(tmp);
}

/* --- Dumps -------------------------------------------------------------- */

void dump_init(Dump *d)
{
    strmap_init(&d->modules);
}

ModuleRecords *dump_entry(Dump *d, const char *file)
{
    ModuleRecords *m = (ModuleRecords *)strmap_get(&d->modules, file);
    if (m == NULL) {
        m = (ModuleRecords *)xmalloc(sizeof *m);
        m->error = NULL;
        occvec_init(&m->occs);
        strmap_put(&d->modules, xstrdup(file), m);
    }
    return m;
}

/* Parse a u32 as Rust's str::parse does: an optional `+`, then decimal
 * digits. Returns NULL on success, or an error message. */
static char *number(const char *s, unsigned long *out)
{
    const char *p = s;
    unsigned long n = 0;
    if (*p == '+') {
        p++;
    }
    if (*p == '\0') {
        return str_printf("not a number: %s", s);
    }
    for (; *p != '\0'; p++) {
        unsigned long d;
        if (*p < '0' || *p > '9') {
            return str_printf("not a number: %s", s);
        }
        d = (unsigned long)(*p - '0');
        if (n > (4294967295UL - d) / 10) {
            return str_printf("not a number: %s", s);
        }
        n = n * 10 + d;
    }
    *out = n;
    return NULL;
}

static char *parse_line(const char *line, Dump *dump)
{
    Vec f = str_split(line, '\t');
    char **fs = (char **)f.items;
    size_t n = f.len;
    if (n == 3 && str_eq(fs[0], "module") && str_eq(fs[2], "ok")) {
        dump_entry(dump, fs[1]);
        return NULL;
    }
    if (n == 4 && str_eq(fs[0], "module") && str_eq(fs[2], "error")) {
        dump_entry(dump, fs[1])->error = fs[3];
        return NULL;
    }
    if (n >= 7 && str_eq(fs[0], "name")) {
        Occurrence o;
        char *e;
        size_t rest = n - 7;
        char **t = fs + 7;
        if ((e = number(fs[2], &o.span.start_line)) != NULL
            || (e = number(fs[3], &o.span.start_col)) != NULL
            || (e = number(fs[4], &o.span.end_line)) != NULL
            || (e = number(fs[5], &o.span.end_col)) != NULL) {
            return e;
        }
        if (str_eq(fs[6], "term")) {
            o.ns = NS_TERM;
        } else if (str_eq(fs[6], "type")) {
            o.ns = NS_TYPE;
        } else if (str_eq(fs[6], "module")) {
            o.ns = NS_MODULE;
        } else {
            return str_printf("unknown namespace %s", fs[6]);
        }
        memset(&o.target, 0, sizeof o.target);
        if (rest == 4 && str_eq(t[0], "top")) {
            o.target.kind = TARGET_TOP;
            o.target.package = t[1];
            o.target.module = t[2];
            o.target.name = t[3];
        } else if (rest == 2 && str_eq(t[0], "local")) {
            o.target.kind = TARGET_LOCAL;
            if ((e = number(t[1], &o.target.id)) != NULL) {
                return e;
            }
        } else if (rest == 1 && str_eq(t[0], "syntax")) {
            o.target.kind = TARGET_SYNTAX;
        } else if (rest == 2 && str_eq(t[0], "error")) {
            o.target.kind = TARGET_ERROR;
            o.target.message = t[1];
        } else {
            return xstrdup("unknown target");
        }
        occvec_push(&dump_entry(dump, fs[1])->occs, o);
        return NULL;
    }
    return xstrdup("unknown record");
}

char *dump_parse(const char *text, Dump *out)
{
    Vec lines = str_lines(text);
    size_t i;
    dump_init(out);
    for (i = 0; i < lines.len; i++) {
        const char *line = (const char *)lines.items[i];
        char *trimmed = str_trim(line);
        char *e;
        int empty = trimmed[0] == '\0';
        free(trimmed);
        if (empty) {
            continue;
        }
        e = parse_line(line, out);
        if (e != NULL) {
            return str_printf("line %lu: %s: %s", (unsigned long)(i + 1), e, line);
        }
    }
    for (i = 0; i < out->modules.len; i++) {
        occvec_sort(&((ModuleRecords *)out->modules.vals[i])->occs);
    }
    return NULL;
}

void dump_write(const Dump *d, StrBuf *out)
{
    size_t i;
    size_t j;
    for (i = 0; i < d->modules.len; i++) {
        const char *file = d->modules.keys[i];
        const ModuleRecords *m = (const ModuleRecords *)d->modules.vals[i];
        if (m->error == NULL) {
            sb_printf(out, "module\t%s\tok\n", file);
        } else {
            sb_printf(out, "module\t%s\terror\t%s\n", file, m->error);
        }
        for (j = 0; j < m->occs.len; j++) {
            const Occurrence *o = &m->occs.items[j];
            sb_printf(out, "name\t%s\t%lu\t%lu\t%lu\t%lu\t%s\t", file,
                      o->span.start_line, o->span.start_col, o->span.end_line,
                      o->span.end_col, namespace_str(o->ns));
            target_write(out, &o->target);
            sb_putc(out, '\n');
        }
    }
}
