/* The layout rule: insert virtual braces and semicolons.
 *
 * This is the algorithm L from section 10.3 of the Haskell 2010 report,
 * run as one pass over the token stream. The report leaves one rule to
 * the parser: an implicit block closes when the next token would
 * otherwise be a parse error. The parser applies that rule (see
 * parse.c). This pass applies two cases of it early, because they make
 * the token stream easier to scan ahead in:
 *
 * - A closing bracket (`)`, `]` or a non-layout `}`) closes every
 *   implicit block that opened inside the bracket.
 * - `where` at the column of a block closes the block.
 */

#include "syntax.h"

/* A layout context. */
typedef struct {
    int implicit;      /* 1: an implicit block; 0: explicit braces */
    unsigned long col; /* implicit blocks: the column of the first token */
    unsigned long depth; /* the bracket depth when the block opened */
    int nosemi;        /* the alternatives of a multi-way if: no `;` */
} Context;

typedef struct {
    Context *items;
    size_t len;
    size_t cap;
} Stack;

static void push(Stack *s, int implicit, unsigned long col, unsigned long depth)
{
    if (s->len == s->cap) {
        s->cap = s->cap ? s->cap * 2 : 16;
        s->items = (Context *)xrealloc(s->items, s->cap * sizeof(Context));
    }
    s->items[s->len].implicit = implicit;
    s->items[s->len].col = col;
    s->items[s->len].depth = depth;
    s->items[s->len].nosemi = 0;
    s->len++;
}

static Context *last(Stack *s)
{
    return s->len ? &s->items[s->len - 1] : NULL;
}

static Token virtual_token(TokenKind kind, Pos pos)
{
    Token t;
    t.kind = kind;
    t.qual = "";
    t.text = "";
    t.value = NULL;
    t.value_len = 0;
    t.ch = 0;
    t.pos = pos;
    t.end = pos;
    return t;
}

static int is_special(const Token *t, int c)
{
    return t->kind == TOK_SPECIAL && t->ch == (unsigned long)c;
}

static int is_keyword(const Token *t, int k)
{
    return t->kind == TOK_KEYWORD && t->ch == (unsigned long)k;
}

/* The column of the innermost implicit block, or 0 inside explicit
 * braces or at the top. */
static unsigned long enclosing_col(Stack *s)
{
    Context *c = last(s);
    return c && c->implicit ? c->col : 0;
}

/* Close the implicit blocks that opened at bracket depth `depth`. */
static void close_implicit_at(Stack *s, TokenVec *out, unsigned long depth, Pos pos)
{
    Context *c;
    while ((c = last(s)) != NULL && c->implicit) {
        if (c->depth != depth) {
            break;
        }
        s->len--;
        tokvec_push(out, virtual_token(TOK_VCLOSE, pos));
    }
}

/* Apply the layout rule. The input must end with an EOF token; the
 * output does too. */
int layout(const TokenVec *in, TokenVec *out, SyntaxError *err)
{
    Stack stack;
    unsigned long depth = 0;
    /* The previous token was a keyword that opens a block. */
    int pending_open = 0;
    unsigned long prev_line = 0;
    /* The previous token was `\`, so a following `case` opens a block
     * (LambdaCase). */
    int after_backslash = 0;
    /* The previous token was `if`, so a following `|` opens a block of
     * alternatives (MultiWayIf). */
    int after_if = 0;
    size_t i;

    stack.items = NULL;
    stack.len = 0;
    stack.cap = 0;
    out->items = NULL;
    out->len = 0;
    out->cap = 0;

    /* The module body is a block unless it starts with `module` or `{`. */
    for (i = 0; i < in->len; i++) {
        const Token *t = &in->items[i];
        if (t->kind == TOK_PRAGMA) {
            continue;
        }
        if (!is_keyword(t, KW_MODULE) && !is_special(t, '{')) {
            pending_open = 1;
        }
        break;
    }
    if (i == in->len) {
        pending_open = 1;
    }

    for (i = 0; i < in->len; i++) {
        Token tok = in->items[i];
        Pos pos = tok.pos;
        unsigned long col = pos.col;
        int is_eof = tok.kind == TOK_EOF;

        if (tok.kind == TOK_PRAGMA) {
            /* LANGUAGE pragmas are comments for the layout rule. */
            tokvec_push(out, tok);
            continue;
        }

        if (pending_open) {
            unsigned long enclosing, n;
            pending_open = 0;
            if (is_special(&tok, '{')) {
                push(&stack, 0, 0, depth);
                tokvec_push(out, tok);
                prev_line = pos.line;
                after_backslash = 0;
                continue;
            }
            enclosing = enclosing_col(&stack);
            n = is_eof ? 0 : col;
            if (n > enclosing || (stack.len == 0 && n > 0)) {
                push(&stack, 1, n, depth);
                tokvec_push(out, virtual_token(TOK_VOPEN, pos));
                /* The token that opened the block is not the first token
                 * on a new line for the purpose of the <n> rule. */
                prev_line = pos.line;
            } else {
                /* An empty block: `{}`. The token then starts a new line
                 * as usual. */
                tokvec_push(out, virtual_token(TOK_VOPEN, pos));
                tokvec_push(out, virtual_token(TOK_VCLOSE, pos));
            }
        }

        if (after_if && tok.kind == TOK_RESERVEDOP && tok.ch == OP_BAR) {
            push(&stack, 1, col, depth);
            stack.items[stack.len - 1].nosemi = 1;
            tokvec_push(out, virtual_token(TOK_VOPEN, pos));
            prev_line = pos.line;
        }
        after_if = 0;

        if (is_eof) {
            while (stack.len > 0) {
                Context ctx = stack.items[--stack.len];
                if (ctx.implicit) {
                    tokvec_push(out, virtual_token(TOK_VCLOSE, pos));
                } else {
                    err->stage = "layout";
                    err->pos = pos;
                    err->message = xstrdup("missing `}` at end of input");
                    return -1;
                }
            }
            tokvec_push(out, tok);
            break;
        }

        /* The <n> rule: the first token on a line. */
        if (pos.line > prev_line) {
            for (;;) {
                Context *c = last(&stack);
                if (c != NULL && c->implicit && col < c->col) {
                    stack.len--;
                    tokvec_push(out, virtual_token(TOK_VCLOSE, pos));
                } else if (c != NULL && c->implicit && col == c->col && c->nosemi) {
                    break;
                } else if (c != NULL && c->implicit && col == c->col) {
                    /* `where` cannot start a block item. The parse-error
                     * rule closes the block instead, so a `where` at the
                     * column of a `do` block belongs to the equation. */
                    if (is_keyword(&tok, KW_WHERE)) {
                        stack.len--;
                        tokvec_push(out, virtual_token(TOK_VCLOSE, pos));
                        continue;
                    }
                    tokvec_push(out, virtual_token(TOK_VSEMI, pos));
                    break;
                } else {
                    break;
                }
            }
        }
        prev_line = pos.line;

        /* The parse-error rule, approximated. */
        if (is_special(&tok, ')') || is_special(&tok, ']')
            || is_special(&tok, SPECIAL_UCLOSE)) {
            close_implicit_at(&stack, out, depth, pos);
            if (depth > 0) {
                depth--;
            }
        } else if (is_special(&tok, '}')) {
            Context *c;
            close_implicit_at(&stack, out, depth, pos);
            c = last(&stack);
            if (c != NULL && !c->implicit && c->depth == depth) {
                stack.len--;
            } else if (depth > 0) {
                depth--;
            }
        } else if (is_special(&tok, '(') || is_special(&tok, '[') || is_special(&tok, '{')
                   || is_special(&tok, SPECIAL_UOPEN)) {
            depth++;
        }

        /* Keywords that open a block. */
        if (is_keyword(&tok, KW_LET) || is_keyword(&tok, KW_WHERE) ||
            is_keyword(&tok, KW_DO) || is_keyword(&tok, KW_OF)) {
            pending_open = 1;
        } else if (is_keyword(&tok, KW_CASE) && after_backslash) {
            pending_open = 1;
        }
        after_backslash = tok.kind == TOK_RESERVEDOP && tok.ch == OP_BACKSLASH;
        after_if = is_keyword(&tok, KW_IF);

        tokvec_push(out, tok);
    }
    return 0;
}
