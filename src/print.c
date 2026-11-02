/* The printer: a syntax tree back to Haskell source.
 *
 * The output uses explicit braces and semicolons, so the layout rule
 * has no effect when a parser reads it back. The printer keeps every
 * token of the tree in source order. The round trip through aihc-parser
 * checks this: its tree for the printed source must be equal to its tree
 * for the original.
 *
 * The printer has one case for each kind of tree node, as print.rs has
 * one match arm for each node.
 */

#include <string.h>

#include "syntax.h"

static void pr_export(StrBuf *out, const Export *e);
static void pr_ty(StrBuf *out, const Type *t);
static void pr_decl(StrBuf *out, const Decl *d);
static void pr_pat(StrBuf *out, const Pat *p);
static void pr_exp(StrBuf *out, const Expr *e);
static void pr_stmt(StrBuf *out, const Stmt *s);
static void pr_decl_block(StrBuf *out, const Vec *decls);
static void pr_con_decl(StrBuf *out, const ConDecl *c);
static void pr_deriving(StrBuf *out, const Deriving *d);
static void pr_alts(StrBuf *out, const Vec *alts);
static void pr_subs(StrBuf *out, const Subs *subs);
static void pr_import(StrBuf *out, const Import *imp);

/* Whether a name is an operator. An operator needs parentheses where a
 * variable is expected, and none where an operator is expected. */
static int is_symbolic(const char *name)
{
    const char *p = name;
    unsigned long c;

    if (*p == '\0')
        return 0;
    c = utf8_next(&p);
    return !uc_is_alphanumeric(c) && c != '_' && c != '(' && c != '[';
}

/* Append a name with its qualifier. The qualifier goes inside a leading
 * parenthesis or backquote. */
static void qualify(StrBuf *out, const char *qual, const char *name)
{
    if (qual[0] == '\0') {
        sb_puts(out, name);
    } else if (name[0] == '(' || name[0] == '`') {
        sb_putc(out, name[0]);
        sb_puts(out, qual);
        sb_putc(out, '.');
        sb_puts(out, name + 1);
    } else {
        sb_puts(out, qual);
        sb_putc(out, '.');
        sb_puts(out, name);
    }
}

/* Append a name in a position that expects a variable or constructor. */
static void var_name(StrBuf *out, const char *qual, const char *name)
{
    char *n;

    if (is_symbolic(name))
        n = str_concat3("(", name, ")");
    else
        n = xstrdup(name);
    qualify(out, qual, n);
}

/* Append a name in a position that expects an operator. */
static void op_name(StrBuf *out, const QName *name)
{
    char *n;

    if (is_symbolic(name->name))
        n = xstrdup(name->name);
    else
        n = str_concat3("`", name->name, "`");
    qualify(out, name->qual, n);
}

/* Append a type constructor name. The special names `()`, `[]`, `(,)`
 * and `(->)` keep their spelling. Operators get parentheses. */
static void con_name(StrBuf *out, const QName *name)
{
    const char *n = name->name;

    if (str_eq(n, "()") || str_eq(n, "[]"))
        sb_puts(out, n);
    else if (str_eq(n, "->"))
        sb_puts(out, "(->)");
    else if (str_starts_with(n, "(,"))
        sb_puts(out, n);
    else
        var_name(out, name->qual, n);
}

/* Append one character, escaped for a literal that `quote` delimits.
 * Returns whether the escape is numeric. Then a digit that follows
 * needs `\&`. */
static int escape_char(unsigned long c, unsigned long quote, StrBuf *out,
                       int prev_numeric)
{
    if (prev_numeric && c >= '0' && c <= '9')
        sb_puts(out, "\\&");
    if (c == '\\') {
        sb_puts(out, "\\\\");
    } else if (c == '\n') {
        sb_puts(out, "\\n");
    } else if (c == '\t') {
        sb_puts(out, "\\t");
    } else if (c == '\r') {
        sb_puts(out, "\\r");
    } else if (c == quote) {
        sb_putc(out, '\\');
        sb_putc(out, (int)c);
    } else if (c >= ' ' && c <= '~') {
        sb_putc(out, (int)c);
    } else {
        sb_putc(out, '\\');
        sb_putul(out, c);
        return 1;
    }
    return 0;
}

/* A string literal in Haskell syntax. */
char *string_literal(const char *s, size_t len)
{
    StrBuf out;
    unsigned long *cs;
    size_t n, i;
    int prev_numeric = 0;

    sb_init(&out);
    sb_putc(&out, '"');
    n = utf8_decode(s, len, &cs);
    for (i = 0; i < n; i++)
        prev_numeric = escape_char(cs[i], '"', &out, prev_numeric);
    sb_putc(&out, '"');
    return sb_take(&out);
}

/* A character literal in Haskell syntax. */
char *char_literal(unsigned long c)
{
    StrBuf out;

    sb_init(&out);
    sb_putc(&out, '\'');
    escape_char(c, '\'', &out, 0);
    sb_putc(&out, '\'');
    return sb_take(&out);
}


/* Append strings with ", " between them. */
static void join(StrBuf *out, const Vec *strs)
{
    size_t i;

    for (i = 0; i < strs->len; i++) {
        if (i > 0)
            sb_puts(out, ", ");
        sb_puts(out, strs->items[i]);
    }
}

/* Print a module as source text. */
char *print_module(const Module *m)
{
    StrBuf out;
    size_t i;
    int first = 1;

    sb_init(&out);
    for (i = 0; i < m->pragmas.len; i++)
        sb_printf(&out, "{-# %s #-}\n", (const char *)m->pragmas.items[i]);
    sb_puts(&out, "module ");
    sb_puts(&out, m->name);
    if (m->has_exports) {
        sb_puts(&out, " (");
        for (i = 0; i < m->exports.len; i++) {
            if (i > 0)
                sb_puts(&out, ", ");
            pr_export(&out, m->exports.items[i]);
        }
        sb_puts(&out, ")");
    }
    sb_puts(&out, " where {\n");
    for (i = 0; i < m->imports.len; i++) {
        if (!first)
            sb_puts(&out, ";\n");
        first = 0;
        pr_import(&out, m->imports.items[i]);
    }
    for (i = 0; i < m->decls.len; i++) {
        if (!first)
            sb_puts(&out, ";\n");
        first = 0;
        pr_decl(&out, m->decls.items[i]);
    }
    sb_puts(&out, "\n}\n");
    return sb_take(&out);
}

static void pr_export(StrBuf *out, const Export *e)
{
    switch (e->kind) {
    case EXPORT_VAR:
        var_name(out, e->name.qual, e->name.name);
        break;
    case EXPORT_THING:
        if (e->explicit_type)
            sb_puts(out, "type ");
        var_name(out, e->name.qual, e->name.name);
        pr_subs(out, &e->subs);
        break;
    case EXPORT_PATTERN:
        sb_puts(out, "pattern ");
        var_name(out, e->name.qual, e->name.name);
        break;
    case EXPORT_MODULE:
        sb_puts(out, "module ");
        sb_puts(out, e->module);
        break;
    }
}

/* Append unqualified names (char *) with ", " between them. */
static void names(StrBuf *out, const Vec *ns)
{
    size_t i;

    for (i = 0; i < ns->len; i++) {
        if (i > 0)
            sb_puts(out, ", ");
        var_name(out, "", ns->items[i]);
    }
}

/* Append the names of identifiers (Ident *), as names does. */
static void ident_names(StrBuf *out, const Vec *ns)
{
    size_t i;

    for (i = 0; i < ns->len; i++) {
        const Ident *id = ns->items[i];

        if (i > 0)
            sb_puts(out, ", ");
        var_name(out, "", id->name);
    }
}

static void pr_subs(StrBuf *out, const Subs *subs)
{
    switch (subs->kind) {
    case SUBS_NONE:
        break;
    case SUBS_ALL:
        sb_puts(out, "(..)");
        break;
    case SUBS_SOME:
        sb_puts(out, "(");
        names(out, &subs->names);
        sb_puts(out, ")");
        break;
    case SUBS_ALL_AND:
        sb_puts(out, "(.., ");
        names(out, &subs->names);
        sb_puts(out, ")");
        break;
    }
}

static void pr_import(StrBuf *out, const Import *imp)
{
    size_t k;

    sb_puts(out, "import ");
    if (imp->qualified == QUAL_PRE)
        sb_puts(out, "qualified ");
    if (imp->package) {
        sb_puts(out, string_literal(imp->package, strlen(imp->package)));
        sb_puts(out, " ");
    }
    sb_puts(out, imp->module);
    if (imp->qualified == QUAL_POST)
        sb_puts(out, " qualified");
    if (imp->alias) {
        sb_puts(out, " as ");
        sb_puts(out, imp->alias);
    }
    if (imp->hiding)
        sb_puts(out, " hiding");
    if (imp->has_items) {
        sb_puts(out, " (");
        for (k = 0; k < imp->items.len; k++) {
            const ImportItem *item = imp->items.items[k];

            if (k > 0)
                sb_puts(out, ", ");
            switch (item->kind) {
            case IMPORT_VAR:
                var_name(out, "", item->name);
                break;
            case IMPORT_THING:
                if (item->explicit_type)
                    sb_puts(out, "type ");
                var_name(out, "", item->name);
                pr_subs(out, &item->subs);
                break;
            case IMPORT_PATTERN:
                sb_puts(out, "pattern ");
                var_name(out, "", item->name);
                break;
            }
        }
        sb_puts(out, ")");
    }
}

/* --- Types --------------------------------------------------------------- */

static void pr_types(StrBuf *out, const Vec *items, const char *sep)
{
    size_t i;

    for (i = 0; i < items->len; i++) {
        if (i > 0)
            sb_puts(out, sep);
        pr_ty(out, items->items[i]);
    }
}

static void pr_literal(StrBuf *out, const Literal *lit)
{
    sb_puts(out, lit->raw);
}

/* Append binders with a leading space each, so that they follow a
 * keyword. */
static void pr_binders(StrBuf *out, const Vec *binders)
{
    size_t i;

    for (i = 0; i < binders->len; i++) {
        const TyVarBind *b = binders->items[i];

        sb_puts(out, " ");
        if (b->kind == NULL) {
            sb_puts(out, b->name.name);
        } else {
            sb_puts(out, "(");
            sb_puts(out, b->name.name);
            sb_puts(out, " :: ");
            pr_ty(out, b->kind);
            sb_puts(out, ")");
        }
    }
}

static void pr_ty(StrBuf *out, const Type *t)
{
    switch (t->kind) {
    case TY_VAR:
        sb_puts(out, t->var.name);
        break;
    case TY_CON:
        con_name(out, &t->con);
        break;
    case TY_APP:
        pr_ty(out, t->a);
        sb_puts(out, " ");
        pr_ty(out, t->b);
        break;
    case TY_APP_KIND:
        pr_ty(out, t->a);
        sb_puts(out, " @");
        pr_ty(out, t->b);
        break;
    case TY_FUN:
        pr_ty(out, t->a);
        sb_puts(out, " -> ");
        pr_ty(out, t->b);
        break;
    case TY_OP:
        pr_ty(out, t->a);
        sb_puts(out, " ");
        op_name(out, &t->con);
        sb_puts(out, " ");
        pr_ty(out, t->b);
        break;
    case TY_LIST:
        sb_puts(out, "[");
        pr_ty(out, t->a);
        sb_puts(out, "]");
        break;
    case TY_TUPLE:
        sb_puts(out, "(");
        pr_types(out, &t->items, ", ");
        sb_puts(out, ")");
        break;
    case TY_PAREN:
        sb_puts(out, "(");
        pr_ty(out, t->a);
        sb_puts(out, ")");
        break;
    case TY_FORALL:
        sb_puts(out, "forall");
        pr_binders(out, &t->binders);
        sb_puts(out, ". ");
        pr_ty(out, t->a);
        break;
    case TY_QUAL:
        pr_ty(out, t->a);
        sb_puts(out, " => ");
        pr_ty(out, t->b);
        break;
    case TY_KIND_SIG:
        pr_ty(out, t->a);
        sb_puts(out, " :: ");
        pr_ty(out, t->b);
        break;
    case TY_PROMOTED:
        sb_puts(out, "'");
        pr_ty(out, t->a);
        break;
    case TY_PROMOTED_LIST:
        sb_puts(out, "'[");
        pr_types(out, &t->items, ", ");
        sb_puts(out, "]");
        break;
    case TY_LIT:
        pr_literal(out, &t->lit);
        break;
    case TY_WILDCARD:
        sb_puts(out, "_");
        break;
    }
}

static void pr_bang_type(StrBuf *out, const BangType *bt)
{
    if (bt->strict == 1)
        sb_puts(out, "!");
    else if (bt->strict == 2)
        sb_puts(out, "~");
    pr_ty(out, bt->ty);
}

/* --- Declarations -------------------------------------------------------- */

/* Append a `where` block with explicit braces. */
static void pr_body(StrBuf *out, const Vec *decls)
{
    sb_puts(out, " where");
    pr_decl_block(out, decls);
}

/* Append ` { d; d }`. */
static void pr_decl_block(StrBuf *out, const Vec *decls)
{
    size_t i;

    sb_puts(out, " {");
    for (i = 0; i < decls->len; i++) {
        if (i > 0)
            sb_puts(out, ";");
        sb_puts(out, "\n  ");
        pr_decl(out, decls->items[i]);
    }
    sb_puts(out, "}");
}

static void pr_lhs(StrBuf *out, const Lhs *lhs);
static void pr_rhs(StrBuf *out, const Rhs *rhs, const char *sep);

static void pr_decl(StrBuf *out, const Decl *d)
{
    size_t i;

    switch (d->kind) {
    case DECL_FOREIGN:
        sb_puts(out, "foreign import ");
        sb_puts(out, d->callconv);
        sb_putc(out, ' ');
        if (d->safety != NULL) {
            sb_puts(out, d->safety);
            sb_putc(out, ' ');
        }
        if (d->entity != NULL) {
            pr_literal(out, d->entity);
            sb_putc(out, ' ');
        }
        var_name(out, "", d->name.name);
        sb_puts(out, " :: ");
        pr_ty(out, d->ty);
        break;
    case DECL_TYPE_SIG:
        ident_names(out, &d->names);
        sb_puts(out, " :: ");
        pr_ty(out, d->ty);
        break;
    case DECL_DEFAULT_SIG:
        sb_puts(out, "default ");
        var_name(out, "", d->name.name);
        sb_puts(out, " :: ");
        pr_ty(out, d->ty);
        break;
    case DECL_PAT_SYN_SIG:
        sb_puts(out, "pattern ");
        join(out, &d->strs);
        sb_puts(out, " :: ");
        pr_ty(out, d->ty);
        break;
    case DECL_FIXITY:
        switch (d->assoc) {
        case ASSOC_LEFT:
            sb_puts(out, "infixl");
            break;
        case ASSOC_RIGHT:
            sb_puts(out, "infixr");
            break;
        case ASSOC_NONE:
            sb_puts(out, "infix");
            break;
        }
        if (d->prec) {
            sb_puts(out, " ");
            sb_puts(out, d->prec);
        }
        sb_puts(out, " ");
        join(out, &d->strs);
        break;
    case DECL_DATA:
        sb_puts(out, d->newtype ? "newtype " : "data ");
        var_name(out, "", d->name.name);
        pr_binders(out, &d->vars);
        if (d->kind_sig) {
            sb_puts(out, " :: ");
            pr_ty(out, d->kind_sig);
        }
        for (i = 0; i < d->cons.len; i++) {
            sb_puts(out, i == 0 ? " = " : " | ");
            pr_con_decl(out, d->cons.items[i]);
        }
        for (i = 0; i < d->deriving.len; i++)
            pr_deriving(out, d->deriving.items[i]);
        break;
    case DECL_TYPE_SYN:
        sb_puts(out, "type ");
        sb_puts(out, d->name.name);
        pr_binders(out, &d->vars);
        sb_puts(out, " = ");
        pr_ty(out, d->ty);
        break;
    case DECL_TYPE_FAMILY:
        sb_puts(out, "type ");
        sb_puts(out, d->name.name);
        pr_binders(out, &d->vars);
        if (d->kind_sig) {
            sb_puts(out, " :: ");
            pr_ty(out, d->kind_sig);
        }
        break;
    case DECL_TYPE_INSTANCE:
        sb_puts(out, "type ");
        pr_ty(out, d->lhs_ty);
        sb_puts(out, " = ");
        pr_ty(out, d->ty);
        break;
    case DECL_CLASS:
        sb_puts(out, "class ");
        if (d->ctx) {
            pr_ty(out, d->ctx);
            sb_puts(out, " => ");
        }
        sb_puts(out, d->name.name);
        pr_binders(out, &d->vars);
        pr_body(out, &d->body);
        break;
    case DECL_INSTANCE:
        sb_puts(out, "instance ");
        pr_ty(out, d->ty);
        pr_body(out, &d->body);
        break;
    case DECL_BIND:
        pr_lhs(out, &d->lhs);
        pr_rhs(out, d->rhs, "=");
        break;
    case DECL_PAT_SYN:
        sb_puts(out, "pattern ");
        switch (d->patsyn.kind) {
        case PATSYN_PREFIX:
            sb_puts(out, d->patsyn.name);
            for (i = 0; i < d->patsyn.args.len; i++) {
                sb_puts(out, " ");
                sb_puts(out, d->patsyn.args.items[i]);
            }
            break;
        case PATSYN_INFIX:
            sb_puts(out, d->patsyn.left);
            sb_puts(out, " ");
            sb_puts(out, d->patsyn.name);
            sb_puts(out, " ");
            sb_puts(out, d->patsyn.right);
            break;
        case PATSYN_RECORD:
            sb_puts(out, d->patsyn.name);
            sb_puts(out, " {");
            join(out, &d->patsyn.args);
            sb_puts(out, "}");
            break;
        }
        switch (d->dir) {
        case PATSYN_IMPLICIT:
            sb_puts(out, " = ");
            pr_pat(out, d->pat);
            break;
        case PATSYN_UNIDIRECTIONAL:
            sb_puts(out, " <- ");
            pr_pat(out, d->pat);
            break;
        case PATSYN_EXPLICIT:
            sb_puts(out, " <- ");
            pr_pat(out, d->pat);
            sb_puts(out, " where");
            pr_decl_block(out, &d->body);
            break;
        }
        break;
    }
}

/* --- Bindings ------------------------------------------------------------ */

static void pr_lhs(StrBuf *out, const Lhs *lhs)
{
    size_t i;

    switch (lhs->kind) {
    case LHS_FUN:
        var_name(out, "", lhs->name.name);
        for (i = 0; i < lhs->args.len; i++) {
            sb_puts(out, " ");
            pr_pat(out, lhs->args.items[i]);
        }
        break;
    case LHS_INFIX:
        if (lhs->args.len > 0)
            sb_puts(out, "(");
        pr_pat(out, lhs->left);
        sb_puts(out, " ");
        sb_puts(out, lhs->name.name);
        sb_puts(out, " ");
        pr_pat(out, lhs->right);
        if (lhs->args.len > 0) {
            sb_puts(out, ")");
            for (i = 0; i < lhs->args.len; i++) {
                sb_puts(out, " ");
                pr_pat(out, lhs->args.items[i]);
            }
        }
        break;
    case LHS_PAT:
        pr_pat(out, lhs->pat);
        break;
    }
}

static void pr_stmts(StrBuf *out, const Vec *stmts, const char *sep)
{
    size_t i;

    for (i = 0; i < stmts->len; i++) {
        if (i > 0)
            sb_puts(out, sep);
        pr_stmt(out, stmts->items[i]);
    }
}

/* Append `= e` or guards, then `where`. sep is `=` or `->`. */
static void pr_rhs(StrBuf *out, const Rhs *rhs, const char *sep)
{
    size_t i;

    if (!rhs->guarded) {
        sb_puts(out, " ");
        sb_puts(out, sep);
        sb_puts(out, " ");
        pr_exp(out, rhs->plain);
    } else {
        for (i = 0; i < rhs->guards.len; i++) {
            const GuardedRhs *g = rhs->guards.items[i];

            sb_puts(out, " | ");
            pr_stmts(out, &g->guards, ", ");
            sb_puts(out, " ");
            sb_puts(out, sep);
            sb_puts(out, " ");
            pr_exp(out, g->body);
        }
    }
    if (rhs->wheres.len > 0) {
        sb_puts(out, " where");
        pr_decl_block(out, &rhs->wheres);
    }
}

static void pr_stmt(StrBuf *out, const Stmt *s)
{
    switch (s->kind) {
    case STMT_BIND:
        pr_pat(out, s->pat);
        sb_puts(out, " <- ");
        pr_exp(out, s->expr);
        break;
    case STMT_LET:
        sb_puts(out, "let");
        pr_decl_block(out, &s->decls);
        break;
    case STMT_EXPR:
        pr_exp(out, s->expr);
        break;
    }
}

/* --- Patterns ------------------------------------------------------------ */

static void pr_pats(StrBuf *out, const Vec *pats)
{
    size_t i;

    for (i = 0; i < pats->len; i++) {
        if (i > 0)
            sb_puts(out, ", ");
        pr_pat(out, pats->items[i]);
    }
}

static void pr_pat(StrBuf *out, const Pat *p)
{
    size_t i;

    switch (p->kind) {
    case PAT_VAR:
        var_name(out, "", p->var.name);
        break;
    case PAT_WILDCARD:
        sb_puts(out, "_");
        break;
    case PAT_LIT:
        if (p->negative)
            sb_puts(out, "-");
        pr_literal(out, &p->lit);
        break;
    case PAT_CON:
        con_name(out, &p->con);
        for (i = 0; i < p->args.len; i++) {
            sb_puts(out, " ");
            pr_pat(out, p->args.items[i]);
        }
        break;
    case PAT_INFIX:
        pr_pat(out, p->a);
        for (i = 0; i < p->steps.len; i++) {
            const InfixStep *st = p->steps.items[i];

            sb_puts(out, " ");
            op_name(out, &st->op);
            sb_puts(out, " ");
            pr_pat(out, st->operand);
        }
        break;
    case PAT_RECORD:
        con_name(out, &p->con);
        sb_puts(out, " {");
        for (i = 0; i < p->fields.len; i++) {
            const FieldPat *f = p->fields.items[i];

            if (i > 0)
                sb_puts(out, ", ");
            var_name(out, f->name.qual, f->name.name);
            if (f->pat) {
                sb_puts(out, " = ");
                pr_pat(out, f->pat);
            }
        }
        if (p->wildcard) {
            if (p->fields.len > 0)
                sb_puts(out, ", ");
            sb_puts(out, "..");
        }
        sb_puts(out, "}");
        break;
    case PAT_TUPLE:
        sb_puts(out, "(");
        pr_pats(out, &p->args);
        sb_puts(out, ")");
        break;
    case PAT_LIST:
        sb_puts(out, "[");
        pr_pats(out, &p->args);
        sb_puts(out, "]");
        break;
    case PAT_PAREN:
        sb_puts(out, "(");
        pr_pat(out, p->a);
        sb_puts(out, ")");
        break;
    case PAT_AS:
        sb_puts(out, p->var.name);
        sb_puts(out, "@");
        pr_pat(out, p->a);
        break;
    case PAT_LAZY:
        sb_puts(out, "~");
        pr_pat(out, p->a);
        break;
    case PAT_BANG:
        sb_puts(out, "!");
        pr_pat(out, p->a);
        break;
    case PAT_VIEW:
        pr_exp(out, p->expr);
        sb_puts(out, " -> ");
        pr_pat(out, p->a);
        break;
    case PAT_SIG:
        pr_pat(out, p->a);
        sb_puts(out, " :: ");
        pr_ty(out, p->ty);
        break;
    case PAT_TYPE_ARG:
        sb_puts(out, "@");
        pr_ty(out, p->ty);
        break;
    }
}

/* --- Expressions --------------------------------------------------------- */

static void pr_exps(StrBuf *out, const Vec *items)
{
    size_t i;

    for (i = 0; i < items->len; i++) {
        if (i > 0)
            sb_puts(out, ", ");
        pr_exp(out, items->items[i]);
    }
}

static void pr_field_updates(StrBuf *out, const Vec *fields, int wildcard)
{
    size_t i;

    sb_puts(out, " {");
    for (i = 0; i < fields->len; i++) {
        const FieldUpdate *f = fields->items[i];

        if (i > 0)
            sb_puts(out, ", ");
        var_name(out, f->name.qual, f->name.name);
        if (f->value) {
            sb_puts(out, " = ");
            pr_exp(out, f->value);
        }
    }
    if (wildcard) {
        if (fields->len > 0)
            sb_puts(out, ", ");
        sb_puts(out, "..");
    }
    sb_puts(out, "}");
}

static void pr_exp(StrBuf *out, const Expr *e)
{
    size_t i;

    switch (e->kind) {
    case EXPR_VAR:
        var_name(out, e->name.qual, e->name.name);
        break;
    case EXPR_CON:
        con_name(out, &e->name);
        break;
    case EXPR_LIT:
        pr_literal(out, &e->lit);
        break;
    case EXPR_HOLE:
        sb_puts(out, "_");
        break;
    case EXPR_APP:
        pr_exp(out, e->a);
        sb_puts(out, " ");
        pr_exp(out, e->b);
        break;
    case EXPR_TYPE_APP:
        pr_exp(out, e->a);
        sb_puts(out, " @");
        pr_ty(out, e->ty);
        break;
    case EXPR_INFIX:
        pr_exp(out, e->a);
        for (i = 0; i < e->steps.len; i++) {
            const InfixStep *st = e->steps.items[i];

            sb_puts(out, " ");
            op_name(out, &st->op);
            sb_puts(out, " ");
            pr_exp(out, st->operand);
        }
        break;
    case EXPR_NEG:
        sb_puts(out, "-");
        pr_exp(out, e->a);
        break;
    case EXPR_LAMBDA:
        sb_puts(out, "\\");
        for (i = 0; i < e->pats.len; i++) {
            pr_pat(out, e->pats.items[i]);
            sb_puts(out, " ");
        }
        sb_puts(out, "-> ");
        pr_exp(out, e->a);
        break;
    case EXPR_LAMBDA_CASE:
        sb_puts(out, "\\case");
        pr_alts(out, &e->alts);
        break;
    case EXPR_LET:
        sb_puts(out, "let");
        pr_decl_block(out, &e->decls);
        sb_puts(out, " in ");
        pr_exp(out, e->a);
        break;
    case EXPR_IF:
        sb_puts(out, "if ");
        pr_exp(out, e->a);
        sb_puts(out, " then ");
        pr_exp(out, e->b);
        sb_puts(out, " else ");
        pr_exp(out, e->c);
        break;
    case EXPR_CASE:
        sb_puts(out, "case ");
        pr_exp(out, e->a);
        sb_puts(out, " of");
        pr_alts(out, &e->alts);
        break;
    case EXPR_DO:
        sb_puts(out, "do {");
        for (i = 0; i < e->stmts.len; i++) {
            if (i > 0)
                sb_puts(out, ";");
            sb_puts(out, "\n  ");
            pr_stmt(out, e->stmts.items[i]);
        }
        sb_puts(out, "}");
        break;
    case EXPR_TUPLE:
        sb_puts(out, "(");
        pr_exps(out, &e->items);
        sb_puts(out, ")");
        break;
    case EXPR_TUPLE_SECTION:
        sb_puts(out, "(");
        for (i = 0; i < e->items.len; i++) {
            if (i > 0)
                sb_puts(out, ",");
            if (e->items.items[i])
                pr_exp(out, e->items.items[i]);
        }
        sb_puts(out, ")");
        break;
    case EXPR_LIST:
        sb_puts(out, "[");
        pr_exps(out, &e->items);
        sb_puts(out, "]");
        break;
    case EXPR_ARITH_SEQ:
        sb_puts(out, "[");
        pr_exp(out, e->a);
        if (e->b) {
            sb_puts(out, ", ");
            pr_exp(out, e->b);
        }
        sb_puts(out, " ..");
        if (e->c) {
            sb_puts(out, " ");
            pr_exp(out, e->c);
        }
        sb_puts(out, "]");
        break;
    case EXPR_LIST_COMP:
        sb_puts(out, "[");
        pr_exp(out, e->a);
        sb_puts(out, " | ");
        pr_stmts(out, &e->stmts, ", ");
        sb_puts(out, "]");
        break;
    case EXPR_PAREN:
        sb_puts(out, "(");
        pr_exp(out, e->a);
        sb_puts(out, ")");
        break;
    case EXPR_LEFT_SECTION:
        sb_puts(out, "(");
        pr_exp(out, e->a);
        sb_puts(out, " ");
        op_name(out, &e->name);
        sb_puts(out, ")");
        break;
    case EXPR_RIGHT_SECTION:
        sb_puts(out, "(");
        op_name(out, &e->name);
        sb_puts(out, " ");
        pr_exp(out, e->a);
        sb_puts(out, ")");
        break;
    case EXPR_RECORD_CON:
        con_name(out, &e->name);
        pr_field_updates(out, &e->fields, e->wildcard);
        break;
    case EXPR_RECORD_UPDATE:
        pr_exp(out, e->a);
        pr_field_updates(out, &e->fields, 0);
        break;
    case EXPR_SIG:
        pr_exp(out, e->a);
        sb_puts(out, " :: ");
        pr_ty(out, e->ty);
        break;
    }
}

static void pr_alts(StrBuf *out, const Vec *alts)
{
    size_t i;

    sb_puts(out, " {");
    for (i = 0; i < alts->len; i++) {
        const Alt *alt = alts->items[i];

        if (i > 0)
            sb_puts(out, ";");
        sb_puts(out, "\n  ");
        pr_pat(out, alt->pat);
        pr_rhs(out, alt->rhs, "->");
    }
    sb_puts(out, "}");
}

static void pr_con_decl(StrBuf *out, const ConDecl *c)
{
    size_t i;

    if (c->forall.len > 0) {
        sb_puts(out, "forall");
        pr_binders(out, &c->forall);
        sb_puts(out, ". ");
    }
    if (c->ctx) {
        pr_ty(out, c->ctx);
        sb_puts(out, " => ");
    }
    switch (c->kind) {
    case CON_PREFIX:
        sb_puts(out, c->name.name);
        for (i = 0; i < c->args.len; i++) {
            sb_puts(out, " ");
            pr_bang_type(out, c->args.items[i]);
        }
        break;
    case CON_INFIX:
        pr_bang_type(out, &c->left);
        sb_puts(out, " ");
        sb_puts(out, c->name.name);
        sb_puts(out, " ");
        pr_bang_type(out, &c->right);
        break;
    case CON_RECORD:
        sb_puts(out, c->name.name);
        sb_puts(out, " { ");
        for (i = 0; i < c->fields.len; i++) {
            const Field *f = c->fields.items[i];

            if (i > 0)
                sb_puts(out, ", ");
            ident_names(out, &f->names);
            sb_puts(out, " :: ");
            pr_bang_type(out, &f->ty);
        }
        sb_puts(out, " }");
        break;
    }
}

static void pr_deriving(StrBuf *out, const Deriving *d)
{
    sb_puts(out, " deriving ");
    switch (d->strategy) {
    case STRATEGY_STOCK:
        sb_puts(out, "stock ");
        break;
    case STRATEGY_ANYCLASS:
        sb_puts(out, "anyclass ");
        break;
    case STRATEGY_NEWTYPE:
        sb_puts(out, "newtype ");
        break;
    case STRATEGY_NONE:
        break;
    }
    if (d->parens) {
        sb_puts(out, "(");
        pr_types(out, &d->classes, ", ");
        sb_puts(out, ")");
    } else {
        pr_types(out, &d->classes, ", ");
    }
    if (d->via) {
        sb_puts(out, " via ");
        pr_ty(out, d->via);
    }
}
