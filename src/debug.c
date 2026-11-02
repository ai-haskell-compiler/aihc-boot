/* The syntax tree in the form of Rust's `{:#?}`.
 *
 * `aihc-boot parse` prints this form. The Rust boot compiler derives
 * Debug for its syntax tree, and the tests compare the output of the two
 * compilers byte for byte. Thus this file maps each fat C struct of
 * syntax.h back to the shape of the Rust type in ast.rs.
 *
 * The pretty form of Rust has these rules:
 *
 * - A struct is `Name {`, one `field: value,` line for each field, and
 *   `}`.
 * - A tuple variant is `Name(`, one `value,` line for each value, and
 *   `)`. A tuple has no name.
 * - A unit variant is its name.
 * - A vector is `[`, one `value,` line for each item, and `]`. An empty
 *   vector is `[]`.
 * - An Option is `None`, or the tuple variant `Some(value)`.
 * - Box is transparent.
 * - Each level of nesting adds 4 spaces of indentation.
 */

#include "syntax.h"

/* The state of the printer: the output and the nesting level. */
typedef struct {
    StrBuf b;
    int ind;
} Dbg;

/* An item printer for d_vec. */
typedef void (*ItemFn)(Dbg *d, const void *x);

/* --- Layout ------------------------------------------------------------- */

static void d_indent(Dbg *d)
{
    int i;

    for (i = 0; i < d->ind; i++)
        sb_puts(&d->b, "    ");
}

/* Start a struct (`{`), a tuple variant (`(`) or a vector (`[`). */
static void d_open(Dbg *d, const char *name, int ch)
{
    sb_puts(&d->b, name);
    if (ch == '{')
        sb_putc(&d->b, ' ');
    sb_putc(&d->b, ch);
    sb_putc(&d->b, '\n');
    d->ind++;
}

/* End what d_open started. */
static void d_close(Dbg *d, int ch)
{
    d->ind--;
    d_indent(d);
    sb_putc(&d->b, ch);
}

/* Start a field of a struct. The value follows. */
static void d_field(Dbg *d, const char *name)
{
    d_indent(d);
    sb_puts(&d->b, name);
    sb_puts(&d->b, ": ");
}

/* Start an item of a tuple or a vector. The value follows. */
static void d_item(Dbg *d)
{
    d_indent(d);
}

/* End a field or an item. */
static void d_end(Dbg *d)
{
    sb_puts(&d->b, ",\n");
}

/* --- Scalars ------------------------------------------------------------ */

static void d_ul(Dbg *d, unsigned long n)
{
    sb_putul(&d->b, n);
}

static void d_bool(Dbg *d, int v)
{
    sb_puts(&d->b, v ? "true" : "false");
}

/* Whether Rust's Debug escapes a code point above 0x7F with `\u{...}`.
 * Rust escapes grapheme extenders and the code points that are not
 * printable. This test covers the common ranges only. */
static int needs_unicode_escape(unsigned long c)
{
    static const unsigned long ranges[][2] = {
        {0x80, 0xA0},       /* C1 controls, no-break space */
        {0xAD, 0xAD},       /* soft hyphen */
        {0x300, 0x36F},     /* combining diacritical marks */
        {0x483, 0x489},
        {0x591, 0x5BD},
        {0x600, 0x605},
        {0x61C, 0x61C},
        {0x6DD, 0x6DD},
        {0x70F, 0x70F},
        {0x1680, 0x1680},
        {0x180E, 0x180E},
        {0x1AB0, 0x1AFF},
        {0x1DC0, 0x1DFF},
        {0x2000, 0x200F},   /* spaces, zero width characters */
        {0x2028, 0x202F},   /* separators, bidirectional controls */
        {0x205F, 0x2064},
        {0x2066, 0x206F},
        {0x20D0, 0x20FF},   /* combining marks for symbols */
        {0x3000, 0x3000},
        {0xD800, 0xF8FF},   /* surrogates, private use */
        {0xFE00, 0xFE0F},   /* variation selectors */
        {0xFE20, 0xFE2F},
        {0xFEFF, 0xFEFF},
        {0xFFF9, 0xFFFB},
        {0xE0000, 0xE0FFF},
        {0xF0000, 0x10FFFF}
    };
    size_t i;

    for (i = 0; i < sizeof ranges / sizeof ranges[0]; i++)
        if (c >= ranges[i][0] && c <= ranges[i][1])
            return 1;
    return 0;
}

/* Append one code point with the Debug escapes of Rust. `quote` is the
 * delimiter of the literal; only the delimiter gets a backslash. */
static void d_escape(Dbg *d, unsigned long c, unsigned long quote)
{
    static const char hex[] = "0123456789abcdef";
    char digits[8];
    int n = 0;

    if (c == 0) {
        sb_puts(&d->b, "\\0");
    } else if (c == '\t') {
        sb_puts(&d->b, "\\t");
    } else if (c == '\r') {
        sb_puts(&d->b, "\\r");
    } else if (c == '\n') {
        sb_puts(&d->b, "\\n");
    } else if (c == '\\') {
        sb_puts(&d->b, "\\\\");
    } else if (c == quote) {
        sb_putc(&d->b, '\\');
        sb_putc(&d->b, (int)c);
    } else if (c < 0x20 || c == 0x7F || (c > 0x7F && needs_unicode_escape(c))) {
        sb_puts(&d->b, "\\u{");
        do {
            digits[n++] = hex[c & 0xF];
            c >>= 4;
        } while (c != 0);
        while (n > 0)
            sb_putc(&d->b, digits[--n]);
        sb_putc(&d->b, '}');
    } else {
        sb_put_utf8(&d->b, c);
    }
}

/* A Rust String in Debug form. len gives the length in bytes, because
 * the string may hold NUL. */
static void d_strn(Dbg *d, const char *s, size_t len)
{
    unsigned long *cs;
    size_t n, i;

    n = utf8_decode(s, len, &cs);
    sb_putc(&d->b, '"');
    for (i = 0; i < n; i++)
        d_escape(d, cs[i], '"');
    sb_putc(&d->b, '"');
}

static void d_str(Dbg *d, const char *s)
{
    const char *e = s;

    while (*e)
        e++;
    d_strn(d, s, (size_t)(e - s));
}

static void d_char(Dbg *d, unsigned long c)
{
    sb_putc(&d->b, '\'');
    d_escape(d, c, '\'');
    sb_putc(&d->b, '\'');
}

/* --- Compound values ---------------------------------------------------- */

static void d_vec(Dbg *d, const Vec *v, ItemFn f)
{
    size_t i;

    if (v->len == 0) {
        sb_puts(&d->b, "[]");
        return;
    }
    d_open(d, "", '[');
    for (i = 0; i < v->len; i++) {
        d_item(d);
        f(d, v->items[i]);
        d_end(d);
    }
    d_close(d, ']');
}

/* `None`, or `Some(x)` with the item printer f. */
static void d_option(Dbg *d, const void *x, ItemFn f)
{
    if (x == NULL) {
        sb_puts(&d->b, "None");
        return;
    }
    d_open(d, "Some", '(');
    d_item(d);
    f(d, x);
    d_end(d);
    d_close(d, ')');
}

/* A tuple variant with one value. */
static void d_tuple1(Dbg *d, const char *name, ItemFn f, const void *x)
{
    d_open(d, name, '(');
    d_item(d);
    f(d, x);
    d_end(d);
    d_close(d, ')');
}

/* A tuple variant with two values. */
static void d_tuple2(Dbg *d, const char *name, ItemFn f, const void *x,
                     ItemFn g, const void *y)
{
    d_open(d, name, '(');
    d_item(d);
    f(d, x);
    d_end(d);
    d_item(d);
    g(d, y);
    d_end(d);
    d_close(d, ')');
}

/* --- Tree nodes --------------------------------------------------------- */

static void d_type(Dbg *d, const void *x);
static void d_pat(Dbg *d, const void *x);
static void d_expr(Dbg *d, const void *x);
static void d_decl(Dbg *d, const void *x);

static void v_str(Dbg *d, const void *x)
{
    d_str(d, x);
}

static void d_pos(Dbg *d, Pos p)
{
    d_open(d, "Pos", '{');
    d_field(d, "line");
    d_ul(d, p.line);
    d_end(d);
    d_field(d, "col");
    d_ul(d, p.col);
    d_end(d);
    d_close(d, '}');
}

static void d_span(Dbg *d, Span s)
{
    d_open(d, "Span", '{');
    d_field(d, "start");
    d_pos(d, s.start);
    d_end(d);
    d_field(d, "end");
    d_pos(d, s.end);
    d_end(d);
    d_close(d, '}');
}

static void v_span(Dbg *d, const void *x)
{
    d_span(d, *(const Span *)x);
}

static void d_qname(Dbg *d, const void *x)
{
    const QName *q = x;

    d_open(d, "QName", '{');
    d_field(d, "qual");
    d_str(d, q->qual);
    d_end(d);
    d_field(d, "name");
    d_str(d, q->name);
    d_end(d);
    d_field(d, "span");
    d_span(d, q->span);
    d_end(d);
    d_close(d, '}');
}

static void d_ident(Dbg *d, const void *x)
{
    const Ident *id = x;

    d_open(d, "Ident", '{');
    d_field(d, "name");
    d_str(d, id->name);
    d_end(d);
    d_field(d, "span");
    d_span(d, id->span);
    d_end(d);
    d_close(d, '}');
}

static void d_subs(Dbg *d, const Subs *s)
{
    switch (s->kind) {
    case SUBS_NONE:
        sb_puts(&d->b, "None");
        break;
    case SUBS_ALL:
        sb_puts(&d->b, "All");
        break;
    case SUBS_SOME:
        d_open(d, "Some", '(');
        d_item(d);
        d_vec(d, &s->names, v_str);
        d_end(d);
        d_close(d, ')');
        break;
    case SUBS_ALL_AND:
        d_open(d, "AllAnd", '(');
        d_item(d);
        d_vec(d, &s->names, v_str);
        d_end(d);
        d_close(d, ')');
        break;
    }
}

static void d_export(Dbg *d, const void *x)
{
    const Export *e = x;

    switch (e->kind) {
    case EXPORT_VAR:
        d_tuple1(d, "Var", d_qname, &e->name);
        break;
    case EXPORT_THING:
        d_open(d, "Thing", '{');
        d_field(d, "name");
        d_qname(d, &e->name);
        d_end(d);
        d_field(d, "subs");
        d_subs(d, &e->subs);
        d_end(d);
        d_field(d, "explicit_type");
        d_bool(d, e->explicit_type);
        d_end(d);
        d_close(d, '}');
        break;
    case EXPORT_PATTERN:
        d_tuple1(d, "Pattern", d_qname, &e->name);
        break;
    case EXPORT_MODULE:
        d_tuple1(d, "Module", v_str, e->module);
        break;
    }
}

static void d_import_item(Dbg *d, const void *x)
{
    const ImportItem *it = x;

    switch (it->kind) {
    case IMPORT_VAR:
        d_tuple1(d, "Var", v_str, it->name);
        break;
    case IMPORT_THING:
        d_open(d, "Thing", '{');
        d_field(d, "name");
        d_str(d, it->name);
        d_end(d);
        d_field(d, "subs");
        d_subs(d, &it->subs);
        d_end(d);
        d_field(d, "explicit_type");
        d_bool(d, it->explicit_type);
        d_end(d);
        d_close(d, '}');
        break;
    case IMPORT_PATTERN:
        d_tuple1(d, "Pattern", v_str, it->name);
        break;
    }
}

static void v_import_items(Dbg *d, const void *x)
{
    d_vec(d, x, d_import_item);
}

static void d_import(Dbg *d, const void *x)
{
    const Import *imp = x;

    d_open(d, "Import", '{');
    d_field(d, "pos");
    d_pos(d, imp->pos);
    d_end(d);
    d_field(d, "module");
    d_str(d, imp->module);
    d_end(d);
    d_field(d, "qualified");
    sb_puts(&d->b, imp->qualified == QUAL_PRE ? "Pre"
                   : imp->qualified == QUAL_POST ? "Post" : "No");
    d_end(d);
    d_field(d, "package");
    d_option(d, imp->package, v_str);
    d_end(d);
    d_field(d, "alias");
    d_option(d, imp->alias, v_str);
    d_end(d);
    d_field(d, "hiding");
    d_bool(d, imp->hiding);
    d_end(d);
    d_field(d, "items");
    if (imp->has_items)
        d_tuple1(d, "Some", v_import_items, &imp->items);
    else
        sb_puts(&d->b, "None");
    d_end(d);
    d_close(d, '}');
}

/* --- Types --- */

static void d_literal(Dbg *d, const Literal *lit)
{
    switch (lit->kind) {
    case LIT_INTEGER:
        d_tuple1(d, "Integer", v_str, lit->raw);
        break;
    case LIT_FLOAT:
        d_tuple1(d, "Float", v_str, lit->raw);
        break;
    case LIT_CHAR:
        d_open(d, "Char", '{');
        d_field(d, "value");
        d_char(d, lit->ch);
        d_end(d);
        d_field(d, "raw");
        d_str(d, lit->raw);
        d_end(d);
        d_close(d, '}');
        break;
    case LIT_STRING:
        d_open(d, "String", '{');
        d_field(d, "value");
        d_strn(d, lit->value, lit->value_len);
        d_end(d);
        d_field(d, "raw");
        d_str(d, lit->raw);
        d_end(d);
        d_close(d, '}');
        break;
    }
}

static void v_literal(Dbg *d, const void *x)
{
    d_literal(d, x);
}

static void d_tyvarbind(Dbg *d, const void *x)
{
    const TyVarBind *b = x;

    d_open(d, "TyVarBind", '{');
    d_field(d, "name");
    d_ident(d, &b->name);
    d_end(d);
    d_field(d, "kind");
    d_option(d, b->kind, d_type);
    d_end(d);
    d_close(d, '}');
}

/* A tuple variant of two types. */
static void d_type2(Dbg *d, const char *name, const Type *a, const Type *b)
{
    d_tuple2(d, name, d_type, a, d_type, b);
}

static void v_types(Dbg *d, const void *x)
{
    d_vec(d, x, d_type);
}

static void d_type(Dbg *d, const void *x)
{
    const Type *t = x;

    switch (t->kind) {
    case TY_VAR:
        d_tuple1(d, "Var", d_ident, &t->var);
        break;
    case TY_CON:
        d_tuple1(d, "Con", d_qname, &t->con);
        break;
    case TY_APP:
        d_type2(d, "App", t->a, t->b);
        break;
    case TY_APP_KIND:
        d_type2(d, "AppKind", t->a, t->b);
        break;
    case TY_FUN:
        d_type2(d, "Fun", t->a, t->b);
        break;
    case TY_OP:
        d_open(d, "Op", '(');
        d_item(d);
        d_type(d, t->a);
        d_end(d);
        d_item(d);
        d_qname(d, &t->con);
        d_end(d);
        d_item(d);
        d_type(d, t->b);
        d_end(d);
        d_close(d, ')');
        break;
    case TY_LIST:
        d_tuple2(d, "List", d_type, t->a, v_span, &t->span);
        break;
    case TY_TUPLE:
        d_tuple1(d, "Tuple", v_types, &t->items);
        break;
    case TY_PAREN:
        d_tuple2(d, "Paren", d_type, t->a, v_span, &t->span);
        break;
    case TY_FORALL:
        d_open(d, "Forall", '(');
        d_item(d);
        d_vec(d, &t->binders, d_tyvarbind);
        d_end(d);
        d_item(d);
        d_type(d, t->a);
        d_end(d);
        d_close(d, ')');
        break;
    case TY_QUAL:
        d_type2(d, "Qual", t->a, t->b);
        break;
    case TY_KIND_SIG:
        d_type2(d, "KindSig", t->a, t->b);
        break;
    case TY_PROMOTED:
        d_tuple1(d, "Promoted", d_type, t->a);
        break;
    case TY_PROMOTED_LIST:
        d_tuple1(d, "PromotedList", v_types, &t->items);
        break;
    case TY_LIT:
        d_tuple1(d, "Lit", v_literal, &t->lit);
        break;
    case TY_WILDCARD:
        sb_puts(&d->b, "Wildcard");
        break;
    }
}

/* The `bool` of `Some(bool)` in BangType.strict: true for `!`. */
static void v_strict(Dbg *d, const void *x)
{
    d_bool(d, ((const BangType *)x)->strict == 1);
}

static void d_bang_type(Dbg *d, const void *x)
{
    const BangType *bt = x;

    d_open(d, "BangType", '{');
    d_field(d, "strict");
    if (bt->strict == 0)
        sb_puts(&d->b, "None");
    else
        d_tuple1(d, "Some", v_strict, bt);
    d_end(d);
    d_field(d, "ty");
    d_type(d, bt->ty);
    d_end(d);
    d_close(d, '}');
}

/* --- Declarations --- */

static void v_decls(Dbg *d, const void *x)
{
    d_vec(d, x, d_decl);
}

static void v_pats(Dbg *d, const void *x)
{
    d_vec(d, x, d_pat);
}

static void d_field_decl(Dbg *d, const void *x)
{
    const Field *f = x;

    d_open(d, "Field", '{');
    d_field(d, "names");
    d_vec(d, &f->names, d_ident);
    d_end(d);
    d_field(d, "ty");
    d_bang_type(d, &f->ty);
    d_end(d);
    d_close(d, '}');
}

static void d_con_decl(Dbg *d, const void *x)
{
    const ConDecl *c = x;

    d_open(d, "ConDecl", '{');
    d_field(d, "pos");
    d_pos(d, c->pos);
    d_end(d);
    d_field(d, "forall");
    d_vec(d, &c->forall, d_tyvarbind);
    d_end(d);
    d_field(d, "ctx");
    d_option(d, c->ctx, d_type);
    d_end(d);
    d_field(d, "body");
    switch (c->kind) {
    case CON_PREFIX:
        d_open(d, "Prefix", '{');
        d_field(d, "name");
        d_ident(d, &c->name);
        d_end(d);
        d_field(d, "args");
        d_vec(d, &c->args, d_bang_type);
        d_end(d);
        d_close(d, '}');
        break;
    case CON_INFIX:
        d_open(d, "Infix", '{');
        d_field(d, "left");
        d_bang_type(d, &c->left);
        d_end(d);
        d_field(d, "op");
        d_ident(d, &c->name);
        d_end(d);
        d_field(d, "right");
        d_bang_type(d, &c->right);
        d_end(d);
        d_close(d, '}');
        break;
    case CON_RECORD:
        d_open(d, "Record", '{');
        d_field(d, "name");
        d_ident(d, &c->name);
        d_end(d);
        d_field(d, "fields");
        d_vec(d, &c->fields, d_field_decl);
        d_end(d);
        d_close(d, '}');
        break;
    }
    d_end(d);
    d_close(d, '}');
}

/* The strategy in `Some(strategy)`. d_deriving prints `None` for
 * STRATEGY_NONE, so that case does not occur here. */
static void v_strategy(Dbg *d, const void *x)
{
    switch (((const Deriving *)x)->strategy) {
    case STRATEGY_STOCK:
        sb_puts(&d->b, "Stock");
        break;
    case STRATEGY_ANYCLASS:
        sb_puts(&d->b, "Anyclass");
        break;
    case STRATEGY_NEWTYPE:
    case STRATEGY_NONE:
        sb_puts(&d->b, "Newtype");
        break;
    }
}

static void d_deriving(Dbg *d, const void *x)
{
    const Deriving *dv = x;

    d_open(d, "Deriving", '{');
    d_field(d, "strategy");
    d_option(d, dv->strategy == STRATEGY_NONE ? NULL : dv, v_strategy);
    d_end(d);
    d_field(d, "classes");
    d_vec(d, &dv->classes, d_type);
    d_end(d);
    d_field(d, "parens");
    d_bool(d, dv->parens);
    d_end(d);
    d_field(d, "via");
    d_option(d, dv->via, d_type);
    d_end(d);
    d_close(d, '}');
}

/* --- Bindings --- */

static void d_stmt(Dbg *d, const void *x);

static void d_lhs(Dbg *d, const Lhs *lhs)
{
    switch (lhs->kind) {
    case LHS_FUN:
        d_open(d, "Fun", '{');
        d_field(d, "name");
        d_ident(d, &lhs->name);
        d_end(d);
        d_field(d, "args");
        d_vec(d, &lhs->args, d_pat);
        d_end(d);
        d_close(d, '}');
        break;
    case LHS_INFIX:
        d_open(d, "Infix", '{');
        d_field(d, "left");
        d_pat(d, lhs->left);
        d_end(d);
        d_field(d, "op");
        d_ident(d, &lhs->name);
        d_end(d);
        d_field(d, "right");
        d_pat(d, lhs->right);
        d_end(d);
        d_field(d, "args");
        d_vec(d, &lhs->args, d_pat);
        d_end(d);
        d_close(d, '}');
        break;
    case LHS_PAT:
        d_tuple1(d, "Pat", d_pat, lhs->pat);
        break;
    }
}

static void d_guarded_rhs(Dbg *d, const void *x)
{
    const GuardedRhs *g = x;

    d_open(d, "GuardedRhs", '{');
    d_field(d, "guards");
    d_vec(d, &g->guards, d_stmt);
    d_end(d);
    d_field(d, "body");
    d_expr(d, g->body);
    d_end(d);
    d_close(d, '}');
}

static void v_guarded_rhss(Dbg *d, const void *x)
{
    d_vec(d, x, d_guarded_rhs);
}

static void d_rhs(Dbg *d, const Rhs *rhs)
{
    d_open(d, "Rhs", '{');
    d_field(d, "body");
    if (!rhs->guarded)
        d_tuple1(d, "Plain", d_expr, rhs->plain);
    else
        d_tuple1(d, "Guarded", v_guarded_rhss, &rhs->guards);
    d_end(d);
    d_field(d, "wheres");
    d_vec(d, &rhs->wheres, d_decl);
    d_end(d);
    d_close(d, '}');
}

static void d_patsyn_lhs(Dbg *d, const PatSynLhs *l)
{
    switch (l->kind) {
    case PATSYN_PREFIX:
        d_open(d, "Prefix", '{');
        d_field(d, "name");
        d_str(d, l->name);
        d_end(d);
        d_field(d, "args");
        d_vec(d, &l->args, v_str);
        d_end(d);
        d_close(d, '}');
        break;
    case PATSYN_INFIX:
        d_open(d, "Infix", '{');
        d_field(d, "left");
        d_str(d, l->left);
        d_end(d);
        d_field(d, "op");
        d_str(d, l->name);
        d_end(d);
        d_field(d, "right");
        d_str(d, l->right);
        d_end(d);
        d_close(d, '}');
        break;
    case PATSYN_RECORD:
        d_open(d, "Record", '{');
        d_field(d, "name");
        d_str(d, l->name);
        d_end(d);
        d_field(d, "fields");
        d_vec(d, &l->args, v_str);
        d_end(d);
        d_close(d, '}');
        break;
    }
}

/* Start a variant of Decl with its name and its `pos` field. */
static void d_decl_open(Dbg *d, const char *name, const Decl *decl)
{
    d_open(d, name, '{');
    d_field(d, "pos");
    d_pos(d, decl->pos);
    d_end(d);
}

static void d_decl(Dbg *d, const void *x)
{
    const Decl *decl = x;

    switch (decl->kind) {
    case DECL_TYPE_SIG:
        d_decl_open(d, "TypeSig", decl);
        d_field(d, "names");
        d_vec(d, &decl->names, d_ident);
        d_end(d);
        d_field(d, "ty");
        d_type(d, decl->ty);
        d_end(d);
        break;
    case DECL_DEFAULT_SIG:
        d_decl_open(d, "DefaultSig", decl);
        d_field(d, "name");
        d_ident(d, &decl->name);
        d_end(d);
        d_field(d, "ty");
        d_type(d, decl->ty);
        d_end(d);
        break;
    case DECL_PAT_SYN_SIG:
        d_decl_open(d, "PatSynSig", decl);
        d_field(d, "names");
        d_vec(d, &decl->strs, v_str);
        d_end(d);
        d_field(d, "ty");
        d_type(d, decl->ty);
        d_end(d);
        break;
    case DECL_FIXITY:
        d_decl_open(d, "Fixity", decl);
        d_field(d, "assoc");
        sb_puts(&d->b, decl->assoc == ASSOC_LEFT ? "Left"
                       : decl->assoc == ASSOC_RIGHT ? "Right" : "None");
        d_end(d);
        d_field(d, "prec");
        d_option(d, decl->prec, v_str);
        d_end(d);
        d_field(d, "ops");
        d_vec(d, &decl->strs, v_str);
        d_end(d);
        break;
    case DECL_DATA:
        d_decl_open(d, "Data", decl);
        d_field(d, "newtype");
        d_bool(d, decl->newtype);
        d_end(d);
        d_field(d, "name");
        d_ident(d, &decl->name);
        d_end(d);
        d_field(d, "vars");
        d_vec(d, &decl->vars, d_tyvarbind);
        d_end(d);
        d_field(d, "kind");
        d_option(d, decl->kind_sig, d_type);
        d_end(d);
        d_field(d, "cons");
        d_vec(d, &decl->cons, d_con_decl);
        d_end(d);
        d_field(d, "deriving");
        d_vec(d, &decl->deriving, d_deriving);
        d_end(d);
        break;
    case DECL_TYPE_SYN:
        d_decl_open(d, "TypeSyn", decl);
        d_field(d, "name");
        d_ident(d, &decl->name);
        d_end(d);
        d_field(d, "vars");
        d_vec(d, &decl->vars, d_tyvarbind);
        d_end(d);
        d_field(d, "rhs");
        d_type(d, decl->ty);
        d_end(d);
        break;
    case DECL_TYPE_FAMILY:
        d_decl_open(d, "TypeFamily", decl);
        d_field(d, "name");
        d_ident(d, &decl->name);
        d_end(d);
        d_field(d, "vars");
        d_vec(d, &decl->vars, d_tyvarbind);
        d_end(d);
        d_field(d, "kind");
        d_option(d, decl->kind_sig, d_type);
        d_end(d);
        break;
    case DECL_TYPE_INSTANCE:
        d_decl_open(d, "TypeInstance", decl);
        d_field(d, "lhs");
        d_type(d, decl->lhs_ty);
        d_end(d);
        d_field(d, "rhs");
        d_type(d, decl->ty);
        d_end(d);
        break;
    case DECL_CLASS:
        d_decl_open(d, "Class", decl);
        d_field(d, "ctx");
        d_option(d, decl->ctx, d_type);
        d_end(d);
        d_field(d, "name");
        d_ident(d, &decl->name);
        d_end(d);
        d_field(d, "vars");
        d_vec(d, &decl->vars, d_tyvarbind);
        d_end(d);
        d_field(d, "body");
        d_vec(d, &decl->body, d_decl);
        d_end(d);
        break;
    case DECL_INSTANCE:
        d_decl_open(d, "Instance", decl);
        d_field(d, "head");
        d_type(d, decl->ty);
        d_end(d);
        d_field(d, "body");
        d_vec(d, &decl->body, d_decl);
        d_end(d);
        break;
    case DECL_BIND:
        d_decl_open(d, "Bind", decl);
        d_field(d, "lhs");
        d_lhs(d, &decl->lhs);
        d_end(d);
        d_field(d, "rhs");
        d_rhs(d, decl->rhs);
        d_end(d);
        break;
    case DECL_PAT_SYN:
        d_decl_open(d, "PatSyn", decl);
        d_field(d, "lhs");
        d_patsyn_lhs(d, &decl->patsyn);
        d_end(d);
        d_field(d, "dir");
        switch (decl->dir) {
        case PATSYN_UNIDIRECTIONAL:
            sb_puts(&d->b, "Unidirectional");
            break;
        case PATSYN_IMPLICIT:
            sb_puts(&d->b, "Implicit");
            break;
        case PATSYN_EXPLICIT:
            d_tuple1(d, "Explicit", v_decls, &decl->body);
            break;
        }
        d_end(d);
        d_field(d, "pat");
        d_pat(d, decl->pat);
        d_end(d);
        break;
    }
    d_close(d, '}');
}

/* --- Patterns --- */

/* A `(QName, Pat)` or `(QName, Expr)` tuple of an operator chain. */
static void d_step(Dbg *d, const InfixStep *st, ItemFn operand)
{
    d_tuple2(d, "", d_qname, &st->op, operand, st->operand);
}

static void d_pat_step(Dbg *d, const void *x)
{
    d_step(d, x, d_pat);
}

static void d_expr_step(Dbg *d, const void *x)
{
    d_step(d, x, d_expr);
}

static void d_field_pat(Dbg *d, const void *x)
{
    const FieldPat *f = x;

    d_open(d, "FieldPat", '{');
    d_field(d, "name");
    d_qname(d, &f->name);
    d_end(d);
    d_field(d, "pat");
    d_option(d, f->pat, d_pat);
    d_end(d);
    d_close(d, '}');
}

static void d_pat(Dbg *d, const void *x)
{
    const Pat *p = x;

    switch (p->kind) {
    case PAT_VAR:
        d_tuple1(d, "Var", d_ident, &p->var);
        break;
    case PAT_WILDCARD:
        sb_puts(&d->b, "Wildcard");
        break;
    case PAT_LIT:
        d_open(d, "Lit", '{');
        d_field(d, "lit");
        d_literal(d, &p->lit);
        d_end(d);
        d_field(d, "negative");
        d_bool(d, p->negative);
        d_end(d);
        d_close(d, '}');
        break;
    case PAT_CON:
        d_open(d, "Con", '{');
        d_field(d, "name");
        d_qname(d, &p->con);
        d_end(d);
        d_field(d, "args");
        d_vec(d, &p->args, d_pat);
        d_end(d);
        d_close(d, '}');
        break;
    case PAT_INFIX:
        d_open(d, "Infix", '{');
        d_field(d, "first");
        d_pat(d, p->a);
        d_end(d);
        d_field(d, "rest");
        d_vec(d, &p->steps, d_pat_step);
        d_end(d);
        d_close(d, '}');
        break;
    case PAT_RECORD:
        d_open(d, "Record", '{');
        d_field(d, "name");
        d_qname(d, &p->con);
        d_end(d);
        d_field(d, "fields");
        d_vec(d, &p->fields, d_field_pat);
        d_end(d);
        d_field(d, "wildcard");
        d_bool(d, p->wildcard);
        d_end(d);
        d_close(d, '}');
        break;
    case PAT_TUPLE:
        d_tuple1(d, "Tuple", v_pats, &p->args);
        break;
    case PAT_LIST:
        d_tuple1(d, "List", v_pats, &p->args);
        break;
    case PAT_PAREN:
        d_tuple1(d, "Paren", d_pat, p->a);
        break;
    case PAT_AS:
        d_tuple2(d, "As", d_ident, &p->var, d_pat, p->a);
        break;
    case PAT_LAZY:
        d_tuple1(d, "Lazy", d_pat, p->a);
        break;
    case PAT_BANG:
        d_tuple1(d, "Bang", d_pat, p->a);
        break;
    case PAT_VIEW:
        d_tuple2(d, "View", d_expr, p->expr, d_pat, p->a);
        break;
    case PAT_SIG:
        d_tuple2(d, "Sig", d_pat, p->a, d_type, p->ty);
        break;
    case PAT_TYPE_ARG:
        d_tuple1(d, "TypeArg", d_type, p->ty);
        break;
    }
}

/* --- Expressions --- */

static void d_field_update(Dbg *d, const void *x)
{
    const FieldUpdate *f = x;

    d_open(d, "FieldUpdate", '{');
    d_field(d, "name");
    d_qname(d, &f->name);
    d_end(d);
    d_field(d, "value");
    d_option(d, f->value, d_expr);
    d_end(d);
    d_close(d, '}');
}

static void d_alt(Dbg *d, const void *x)
{
    const Alt *alt = x;

    d_open(d, "Alt", '{');
    d_field(d, "pos");
    d_pos(d, alt->pos);
    d_end(d);
    d_field(d, "pat");
    d_pat(d, alt->pat);
    d_end(d);
    d_field(d, "rhs");
    d_rhs(d, alt->rhs);
    d_end(d);
    d_close(d, '}');
}

static void d_stmt(Dbg *d, const void *x)
{
    const Stmt *s = x;

    switch (s->kind) {
    case STMT_BIND:
        d_tuple2(d, "Bind", d_pat, s->pat, d_expr, s->expr);
        break;
    case STMT_LET:
        d_tuple1(d, "Let", v_decls, &s->decls);
        break;
    case STMT_EXPR:
        d_tuple1(d, "Expr", d_expr, s->expr);
        break;
    }
}

static void v_alts(Dbg *d, const void *x)
{
    d_vec(d, x, d_alt);
}

static void v_stmts(Dbg *d, const void *x)
{
    d_vec(d, x, d_stmt);
}

static void v_exprs(Dbg *d, const void *x)
{
    d_vec(d, x, d_expr);
}

static void v_field_updates(Dbg *d, const void *x)
{
    d_vec(d, x, d_field_update);
}

/* An item of Expr::TupleSection: `None`, or `Some(e)`. */
static void d_section_item(Dbg *d, const void *x)
{
    d_option(d, x, d_expr);
}

static void v_section_items(Dbg *d, const void *x)
{
    d_vec(d, x, d_section_item);
}

static void d_expr(Dbg *d, const void *x)
{
    const Expr *e = x;

    switch (e->kind) {
    case EXPR_VAR:
        d_tuple1(d, "Var", d_qname, &e->name);
        break;
    case EXPR_CON:
        d_tuple1(d, "Con", d_qname, &e->name);
        break;
    case EXPR_LIT:
        d_tuple1(d, "Lit", v_literal, &e->lit);
        break;
    case EXPR_APP:
        d_tuple2(d, "App", d_expr, e->a, d_expr, e->b);
        break;
    case EXPR_TYPE_APP:
        d_tuple2(d, "TypeApp", d_expr, e->a, d_type, e->ty);
        break;
    case EXPR_INFIX:
        d_open(d, "Infix", '{');
        d_field(d, "first");
        d_expr(d, e->a);
        d_end(d);
        d_field(d, "rest");
        d_vec(d, &e->steps, d_expr_step);
        d_end(d);
        d_close(d, '}');
        break;
    case EXPR_NEG:
        d_tuple1(d, "Neg", d_expr, e->a);
        break;
    case EXPR_LAMBDA:
        d_tuple2(d, "Lambda", v_pats, &e->pats, d_expr, e->a);
        break;
    case EXPR_LAMBDA_CASE:
        d_tuple1(d, "LambdaCase", v_alts, &e->alts);
        break;
    case EXPR_LET:
        d_tuple2(d, "Let", v_decls, &e->decls, d_expr, e->a);
        break;
    case EXPR_IF:
        d_open(d, "If", '(');
        d_item(d);
        d_expr(d, e->a);
        d_end(d);
        d_item(d);
        d_expr(d, e->b);
        d_end(d);
        d_item(d);
        d_expr(d, e->c);
        d_end(d);
        d_close(d, ')');
        break;
    case EXPR_CASE:
        d_tuple2(d, "Case", d_expr, e->a, v_alts, &e->alts);
        break;
    case EXPR_DO:
        d_tuple1(d, "Do", v_stmts, &e->stmts);
        break;
    case EXPR_TUPLE:
        d_tuple1(d, "Tuple", v_exprs, &e->items);
        break;
    case EXPR_TUPLE_SECTION:
        d_tuple1(d, "TupleSection", v_section_items, &e->items);
        break;
    case EXPR_LIST:
        d_tuple1(d, "List", v_exprs, &e->items);
        break;
    case EXPR_ARITH_SEQ:
        d_open(d, "ArithSeq", '{');
        d_field(d, "from");
        d_expr(d, e->a);
        d_end(d);
        d_field(d, "then");
        d_option(d, e->b, d_expr);
        d_end(d);
        d_field(d, "to");
        d_option(d, e->c, d_expr);
        d_end(d);
        d_close(d, '}');
        break;
    case EXPR_LIST_COMP:
        d_tuple2(d, "ListComp", d_expr, e->a, v_stmts, &e->stmts);
        break;
    case EXPR_PAREN:
        d_tuple1(d, "Paren", d_expr, e->a);
        break;
    case EXPR_LEFT_SECTION:
        d_tuple2(d, "LeftSection", d_expr, e->a, d_qname, &e->name);
        break;
    case EXPR_RIGHT_SECTION:
        d_tuple2(d, "RightSection", d_qname, &e->name, d_expr, e->a);
        break;
    case EXPR_RECORD_CON:
        d_open(d, "RecordCon", '{');
        d_field(d, "name");
        d_qname(d, &e->name);
        d_end(d);
        d_field(d, "fields");
        d_vec(d, &e->fields, d_field_update);
        d_end(d);
        d_field(d, "wildcard");
        d_bool(d, e->wildcard);
        d_end(d);
        d_close(d, '}');
        break;
    case EXPR_RECORD_UPDATE:
        d_tuple2(d, "RecordUpdate", d_expr, e->a, v_field_updates, &e->fields);
        break;
    case EXPR_SIG:
        d_tuple2(d, "Sig", d_expr, e->a, d_type, e->ty);
        break;
    case EXPR_HOLE:
        sb_puts(&d->b, "Hole");
        break;
    }
}

/* --- Module --- */

static void v_exports(Dbg *d, const void *x)
{
    d_vec(d, x, d_export);
}

/* The tree in the form of Rust's `{:#?}`. */
char *debug_module(const Module *m)
{
    Dbg d;

    sb_init(&d.b);
    d.ind = 0;
    d_open(&d, "Module", '{');
    d_field(&d, "pos");
    d_pos(&d, m->pos);
    d_end(&d);
    d_field(&d, "pragmas");
    d_vec(&d, &m->pragmas, v_str);
    d_end(&d);
    d_field(&d, "name");
    d_str(&d, m->name);
    d_end(&d);
    d_field(&d, "exports");
    if (m->has_exports)
        d_tuple1(&d, "Some", v_exports, &m->exports);
    else
        sb_puts(&d.b, "None");
    d_end(&d);
    d_field(&d, "imports");
    d_vec(&d, &m->imports, d_import);
    d_end(&d);
    d_field(&d, "decls");
    d_vec(&d, &m->decls, d_decl);
    d_end(&d);
    d_close(&d, '}');
    return sb_take(&d.b);
}
