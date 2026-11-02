/* The parser: tokens (after layout) to a syntax tree.
 *
 * This is a recursive descent parser over the token stream of tokenize.
 * This file handles the module header, the export list, the imports and
 * the shared machinery. parse_types.c, parse_decls.c, parse_pats.c and
 * parse_exprs.c handle the rest. Nothing is skipped: a construct the
 * parser does not know is an error.
 *
 * The parser completes the layout rule. The layout pass inserts the
 * virtual braces and semicolons that indentation determines. The report
 * adds one rule it cannot apply: an implicit block also closes where the
 * next token would be a parse error. p_block applies it: when an item of
 * an implicit block ends and the next token is neither `;` nor the
 * block's `}`, or when an item cannot start, the block closes there and
 * its virtual `}` is removed from the stream.
 */

#include "parse.h"

#include <string.h>

/* --- Token access ------------------------------------------------------ */

const Token *p_peek(const Parser *p)
{
    /* The stream ends with EOF, and the parser never moves past it. */
    size_t i = p->idx < p->toks->len ? p->idx : p->toks->len - 1;
    return &p->toks->items[i];
}

const Token *p_tok_at(const Parser *p, size_t i)
{
    return i < p->toks->len ? &p->toks->items[i] : NULL;
}

TokenKind p_kind(const Parser *p)
{
    return p_peek(p)->kind;
}

Pos p_pos(const Parser *p)
{
    return p_peek(p)->pos;
}

Span p_token_span(const Parser *p)
{
    Span s;
    s.start = p_peek(p)->pos;
    s.end = p_peek(p)->end;
    return s;
}

Span p_span_from(const Parser *p, Pos start)
{
    Span s;
    s.start = start;
    s.end = start;
    if (p->idx >= 1 && p->idx - 1 < p->toks->len) {
        s.end = p->toks->items[p->idx - 1].end;
    }
    return s;
}

void p_bump(Parser *p)
{
    size_t i = p->idx < p->toks->len ? p->idx : p->toks->len - 1;
    if (p->toks->items[i].kind != TOK_EOF) {
        p->idx++;
    }
}

int tok_is_special(const Token *t, int c)
{
    return t != NULL && t->kind == TOK_SPECIAL && t->ch == (unsigned long)c;
}

int p_at_keyword(const Parser *p, Keyword k)
{
    const Token *t = p_peek(p);
    return t->kind == TOK_KEYWORD && t->ch == (unsigned long)k;
}

int p_at_special(const Parser *p, int c)
{
    return tok_is_special(p_peek(p), c);
}

int p_at_op(const Parser *p, ReservedOp op)
{
    const Token *t = p_peek(p);
    return t->kind == TOK_RESERVEDOP && t->ch == (unsigned long)op;
}

/* An unqualified variable identifier with the given spelling. Used for
 * the words that are not reserved, such as `qualified`. */
int p_at_varid(const Parser *p, const char *name)
{
    const Token *t = p_peek(p);
    return t->kind == TOK_VARID && t->qual[0] == '\0' && str_eq(t->text, name);
}

int p_at_varsym(const Parser *p, const char *sym)
{
    const Token *t = p_peek(p);
    return t->kind == TOK_VARSYM && t->qual[0] == '\0' && str_eq(t->text, sym);
}

int p_eat_keyword(Parser *p, Keyword k)
{
    if (p_at_keyword(p, k)) {
        p_bump(p);
        return 1;
    }
    return 0;
}

int p_eat_special(Parser *p, int c)
{
    if (p_at_special(p, c)) {
        p_bump(p);
        return 1;
    }
    return 0;
}

int p_eat_op(Parser *p, ReservedOp op)
{
    if (p_at_op(p, op)) {
        p_bump(p);
        return 1;
    }
    return 0;
}

int p_eat_varid(Parser *p, const char *name)
{
    if (p_at_varid(p, name)) {
        p_bump(p);
        return 1;
    }
    return 0;
}

int p_eat_kind(Parser *p, TokenKind kind)
{
    if (p_kind(p) == kind) {
        p_bump(p);
        return 1;
    }
    return 0;
}

int p_error(Parser *p, char *message)
{
    p->err.stage = "parse";
    p->err.pos = p_pos(p);
    p->err.message = message;
    return -1;
}

int p_unexpected(Parser *p, const char *expected)
{
    return p_error(p, str_printf("expected %s, found `%s`", expected, token_str(p_peek(p))));
}

int p_expect_op(Parser *p, ReservedOp op, const char *what)
{
    if (p_eat_op(p, op)) {
        return 0;
    }
    return p_unexpected(p, what);
}

int p_expect_keyword(Parser *p, Keyword k)
{
    if (p_eat_keyword(p, k)) {
        return 0;
    }
    return p_unexpected(p, str_printf("`%s`", keyword_str((int)k)));
}

int p_expect_special(Parser *p, int c)
{
    if (p_eat_special(p, c)) {
        return 0;
    }
    return p_unexpected(p, str_printf("`%c`", c));
}

int p_expect_varsym(Parser *p, const char *sym)
{
    if (p_at_varsym(p, sym)) {
        p_bump(p);
        return 0;
    }
    return p_unexpected(p, str_printf("`%s`", sym));
}

static int p_expect_eof(Parser *p)
{
    if (p_kind(p) == TOK_EOF) {
        return 0;
    }
    return p_unexpected(p, "end of input");
}

/* --- Blocks ------------------------------------------------------------ */

/* `{` or a virtual `{`. Sets *explicit to whether the brace was
 * explicit. */
static int open_block(Parser *p, int *explicit)
{
    if (p_eat_special(p, '{')) {
        *explicit = 1;
        return 0;
    }
    if (p_eat_kind(p, TOK_VOPEN)) {
        *explicit = 0;
        return 0;
    }
    return p_unexpected(p, "`{`");
}

static int at_close_block(const Parser *p, int explicit)
{
    return explicit ? p_at_special(p, '}') : p_kind(p) == TOK_VCLOSE;
}

static void remove_token(TokenVec *v, size_t i)
{
    memmove(v->items + i, v->items + i + 1, (v->len - i - 1) * sizeof(Token));
    v->len--;
}

/* Close a block. An implicit block closes at any token: that is the
 * parse-error rule of the layout algorithm. Its virtual `}` further on in
 * the stream goes away, so the enclosing blocks stay balanced. */
static int close_block(Parser *p, int explicit)
{
    unsigned long depth = 0;
    size_t i;
    if (at_close_block(p, explicit)) {
        p_bump(p);
        return 0;
    }
    if (explicit) {
        return p_unexpected(p, "`}`");
    }
    /* The block's own virtual `;` tokens further on are stale too: a
     * layout-aware lexer stops producing them once the block is closed. */
    i = p->idx;
    while (i < p->toks->len) {
        TokenKind k = p->toks->items[i].kind;
        if (k == TOK_VOPEN) {
            depth++;
        } else if (k == TOK_VSEMI && depth == 0) {
            remove_token(p->toks, i);
            continue;
        } else if (k == TOK_VCLOSE && depth == 0) {
            remove_token(p->toks, i);
            return 0;
        } else if (k == TOK_VCLOSE) {
            depth--;
        }
        i++;
    }
    return p_unexpected(p, "`}`");
}

/* One or more `;` or virtual `;`. Returns whether any was found. */
static int semis(Parser *p)
{
    int found = 0;
    while (p_eat_special(p, ';') || p_eat_kind(p, TOK_VSEMI)) {
        found = 1;
    }
    return found;
}

/* A block of items: `{` or a virtual `{`, items separated by `;`, then
 * `}`. See the comment at the top for how an implicit block closes
 * early. */
int p_block(Parser *p, ItemFn item, void *env, Vec *items)
{
    int explicit = 0;
    vec_init(items);
    if (open_block(p, &explicit) != 0) {
        return -1;
    }
    semis(p);
    while (!at_close_block(p, explicit)) {
        size_t start = p->idx;
        Pos pos = p_pos(p);
        void *x = item(p, env);
        if (x != NULL) {
            vec_push(items, x);
        } else if (!explicit && pos_cmp(p->err.pos, pos) == 0) {
            /* An item that cannot start closes an implicit block. */
            p->idx = start;
            break;
        } else {
            return -1;
        }
        if (!semis(p)) {
            break;
        }
    }
    return close_block(p, explicit);
}

/* --- Names ------------------------------------------------------------- */

static char *modid(Parser *p)
{
    const Token *t = p_peek(p);
    char *m;
    if (t->kind != TOK_CONID) {
        p_unexpected(p, "a module name");
        return NULL;
    }
    m = t->qual[0] == '\0' ? xstrdup(t->text) : str_concat3(t->qual, ".", t->text);
    p_bump(p);
    return m;
}

/* `(varsym)` or `(consym)`, possibly qualified. Returns NULL and consumes
 * nothing when the parenthesis starts something else. */
QName *p_parenthesized_name(Parser *p)
{
    Pos start;
    const Token *t1, *t2;
    QName *name;
    Span none;
    if (!p_at_special(p, '(')) {
        return NULL;
    }
    start = p_pos(p);
    t1 = p_tok_at(p, p->idx + 1);
    memset(&none, 0, sizeof none);
    if (t1 != NULL && (t1->kind == TOK_VARSYM || t1->kind == TOK_CONSYM)) {
        name = qname_new(t1->qual, t1->text, none);
    } else if (t1 != NULL && t1->kind == TOK_RESERVEDOP &&
               (t1->ch == OP_COLON || t1->ch == OP_TILDE)) {
        name = qname_new("", reservedop_str((int)t1->ch), none);
    } else {
        return NULL;
    }
    t2 = p_tok_at(p, p->idx + 2);
    if (!tok_is_special(t2, ')')) {
        return NULL;
    }
    p->idx += 3;
    name->span = p_span_from(p, start);
    return name;
}

/* A possibly qualified variable, constructor or parenthesized operator. */
QName *p_qname(Parser *p)
{
    QName *name = p_parenthesized_name(p);
    const Token *t;
    if (name != NULL) {
        return name;
    }
    t = p_peek(p);
    if (t->kind == TOK_VARID || t->kind == TOK_CONID) {
        name = qname_new(t->qual, t->text, p_token_span(p));
        p_bump(p);
        return name;
    }
    p_unexpected(p, "a name");
    return NULL;
}

/* The token after the current one starts a name, so the current
 * `pattern` or `type` is a namespace keyword and not a name itself. */
static int next_starts_name(const Parser *p)
{
    const Token *t = p_tok_at(p, p->idx + 1);
    return t != NULL && (t->kind == TOK_VARID || t->kind == TOK_CONID || tok_is_special(t, '('));
}

/* Whether a name starts with a character that is neither upper case nor
 * `:`: the name of a variable or a variable operator. */
static int is_var_name(const char *name)
{
    const char *s = name;
    unsigned long c;
    if (*s == '\0') {
        return 0;
    }
    c = utf8_next(&s);
    return !uc_is_uppercase(c) && c != ':';
}

/* The optional `(..)`, `(a, B, (+))` or `(.., P)` after a type or
 * class. */
static int subs(Parser *p, Subs *out)
{
    int all;
    vec_init(&out->names);
    out->kind = SUBS_NONE;
    if (!p_at_special(p, '(')) {
        return 0;
    }
    p_bump(p);
    all = p_eat_op(p, OP_DOTDOT);
    for (;;) {
        QName *q;
        while (p_eat_special(p, ',')) {
        }
        if (p_eat_special(p, ')')) {
            break;
        }
        q = p_qname(p);
        if (q == NULL) {
            return -1;
        }
        vec_push(&out->names, q->name);
        if (!p_at_special(p, ',')) {
            if (p_expect_special(p, ')') != 0) {
                return -1;
            }
            break;
        }
    }
    if (all) {
        out->kind = out->names.len == 0 ? SUBS_ALL : SUBS_ALL_AND;
    } else {
        out->kind = SUBS_SOME;
    }
    return 0;
}

/* --- Module ------------------------------------------------------------ */

static Export *export_item(Parser *p)
{
    Export *e = (Export *)xcalloc(1, sizeof(Export));
    int explicit_type;
    TokenKind k;
    QName *name;
    vec_init(&e->subs.names);
    if (p_eat_keyword(p, KW_MODULE)) {
        e->kind = EXPORT_MODULE;
        e->module = modid(p);
        return e->module ? e : NULL;
    }
    if (p_at_varid(p, "pattern") && next_starts_name(p)) {
        p_bump(p);
        e->kind = EXPORT_PATTERN;
        name = p_qname(p);
        if (name == NULL) {
            return NULL;
        }
        e->name = *name;
        return e;
    }
    explicit_type = p_at_keyword(p, KW_TYPE) && next_starts_name(p);
    if (explicit_type) {
        p_bump(p);
    }
    k = p_kind(p);
    if (k == TOK_VARID && !explicit_type) {
        e->kind = EXPORT_VAR;
        name = p_qname(p);
        if (name == NULL) {
            return NULL;
        }
        e->name = *name;
        return e;
    }
    if (k == TOK_CONID || p_at_special(p, '(')) {
        name = p_qname(p);
        if (name == NULL) {
            return NULL;
        }
        e->name = *name;
        if (!explicit_type && is_var_name(name->name)) {
            e->kind = EXPORT_VAR;
            return e;
        }
        e->kind = EXPORT_THING;
        e->explicit_type = explicit_type;
        if (subs(p, &e->subs) != 0) {
            return NULL;
        }
        return e;
    }
    p_unexpected(p, "an export item");
    return NULL;
}

static int export_list(Parser *p, Vec *items)
{
    vec_init(items);
    if (p_expect_special(p, '(') != 0) {
        return -1;
    }
    for (;;) {
        Export *e;
        /* Trailing and repeated commas are permitted. */
        while (p_eat_special(p, ',')) {
        }
        if (p_eat_special(p, ')')) {
            return 0;
        }
        e = export_item(p);
        if (e == NULL) {
            return -1;
        }
        vec_push(items, e);
        if (!p_at_special(p, ',')) {
            return p_expect_special(p, ')');
        }
    }
}

static ImportItem *import_item(Parser *p)
{
    ImportItem *it = (ImportItem *)xcalloc(1, sizeof(ImportItem));
    int explicit_type;
    TokenKind k;
    QName *name;
    vec_init(&it->subs.names);
    if (p_at_varid(p, "pattern") && next_starts_name(p)) {
        p_bump(p);
        name = p_qname(p);
        if (name == NULL) {
            return NULL;
        }
        it->kind = IMPORT_PATTERN;
        it->name = name->name;
        return it;
    }
    explicit_type = p_at_keyword(p, KW_TYPE) && next_starts_name(p);
    if (explicit_type) {
        p_bump(p);
    }
    k = p_kind(p);
    if (k == TOK_VARID && !explicit_type) {
        name = p_qname(p);
        if (name == NULL) {
            return NULL;
        }
        it->kind = IMPORT_VAR;
        it->name = name->name;
        return it;
    }
    if (k == TOK_CONID || p_at_special(p, '(')) {
        name = p_qname(p);
        if (name == NULL) {
            return NULL;
        }
        it->name = name->name;
        if (!explicit_type && is_var_name(it->name)) {
            it->kind = IMPORT_VAR;
            return it;
        }
        it->kind = IMPORT_THING;
        it->explicit_type = explicit_type;
        if (subs(p, &it->subs) != 0) {
            return NULL;
        }
        return it;
    }
    p_unexpected(p, "an import item");
    return NULL;
}

static int import_list(Parser *p, Vec *items)
{
    vec_init(items);
    if (p_expect_special(p, '(') != 0) {
        return -1;
    }
    for (;;) {
        ImportItem *it;
        while (p_eat_special(p, ',')) {
        }
        if (p_eat_special(p, ')')) {
            return 0;
        }
        it = import_item(p);
        if (it == NULL) {
            return -1;
        }
        vec_push(items, it);
        if (!p_at_special(p, ',')) {
            return p_expect_special(p, ')');
        }
    }
}

static Import *import_decl(Parser *p)
{
    Import *im = (Import *)xcalloc(1, sizeof(Import));
    const Token *t;
    im->pos = p_pos(p);
    vec_init(&im->items);
    if (p_expect_keyword(p, KW_IMPORT) != 0) {
        return NULL;
    }
    im->qualified = p_eat_varid(p, "qualified") ? QUAL_PRE : QUAL_NO;
    t = p_peek(p);
    if (t->kind == TOK_STRING) {
        im->package = xstrndup(t->value, t->value_len);
        p_bump(p);
    }
    im->module = modid(p);
    if (im->module == NULL) {
        return NULL;
    }
    /* ImportQualifiedPost */
    if (p_eat_varid(p, "qualified")) {
        im->qualified = QUAL_POST;
    }
    if (p_eat_varid(p, "as")) {
        im->alias = modid(p);
        if (im->alias == NULL) {
            return NULL;
        }
    }
    im->hiding = p_eat_varid(p, "hiding");
    if (p_at_special(p, '(')) {
        im->has_items = 1;
        if (import_list(p, &im->items) != 0) {
            return NULL;
        }
    } else if (im->hiding) {
        p_unexpected(p, "`(` after `hiding`");
        return NULL;
    }
    return im;
}

/* The state of the module body: imports come before declarations. */
typedef struct {
    Module *m;
    int decls_started;
} BodyEnv;

/* A non-NULL result for an item that is an import. */
static char import_marker;

static void *body_item(Parser *p, void *env)
{
    BodyEnv *b = (BodyEnv *)env;
    if (p_at_keyword(p, KW_IMPORT) && !b->decls_started) {
        Import *im = import_decl(p);
        if (im == NULL) {
            return NULL;
        }
        vec_push(&b->m->imports, im);
        return &import_marker;
    }
    b->decls_started = 1;
    return p_decl(p, CTX_TOP_LEVEL);
}

static Module *module(Parser *p)
{
    Module *m = (Module *)xcalloc(1, sizeof(Module));
    BodyEnv env;
    Vec items;
    size_t i;
    m->pos = p_pos(p);
    vec_init(&m->pragmas);
    vec_init(&m->exports);
    vec_init(&m->imports);
    vec_init(&m->decls);
    while (p_kind(p) == TOK_PRAGMA) {
        vec_push(&m->pragmas, str_trim(p_peek(p)->text));
        p_bump(p);
    }
    if (p_eat_keyword(p, KW_MODULE)) {
        m->name = modid(p);
        if (m->name == NULL) {
            return NULL;
        }
        if (p_at_special(p, '(')) {
            m->has_exports = 1;
            if (export_list(p, &m->exports) != 0) {
                return NULL;
            }
        }
        if (p_expect_keyword(p, KW_WHERE) != 0) {
            return NULL;
        }
    } else {
        m->name = xstrdup("Main");
    }
    env.m = m;
    env.decls_started = 0;
    if (p_block(p, body_item, &env, &items) != 0) {
        return NULL;
    }
    for (i = 0; i < items.len; i++) {
        if (items.items[i] != (void *)&import_marker) {
            vec_push(&m->decls, items.items[i]);
        }
    }
    return m;
}

Module *parse_module(TokenVec *tokens, SyntaxError *err)
{
    Parser p;
    Module *m;
    p.toks = tokens;
    p.idx = 0;
    p.err.stage = "parse";
    p.err.pos.line = 0;
    p.err.pos.col = 0;
    p.err.message = NULL;
    m = module(&p);
    if (m == NULL || p_expect_eof(&p) != 0) {
        *err = p.err;
        return NULL;
    }
    return m;
}

Module *parse_source(const char *src, size_t len, SyntaxError *err)
{
    TokenVec toks;
    if (tokenize(src, len, &toks, err) != 0) {
        return NULL;
    }
    return parse_module(&toks, err);
}
