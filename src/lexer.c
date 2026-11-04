/* The lexer: source text to tokens.
 *
 * The lexer follows chapter 2 of the Haskell 2010 report, plus the
 * extensions the vendored tree uses: MagicHash names and the `'` tick of
 * DataKinds. Comments go away. So do pragmas, except LANGUAGE, which
 * changes how a module is read. aihc-boot ignores every other pragma. The
 * lexer does not apply the layout rule; see layout.c for that.
 */

#include "syntax.h"

#include <string.h>

/* --- Options ----------------------------------------------------------- */

/* The position of needle in haystack, or NULL. */
static const char *find(const char *haystack, const char *needle)
{
    return strstr(haystack, needle);
}

Vec language_pragmas(const char *src)
{
    Vec exts;
    const char *rest = src;
    const char *start, *after, *end;
    vec_init(&exts);
    while ((start = find(rest, "{-#")) != NULL) {
        char *body, *trimmed;
        after = start + 3;
        end = find(after, "#-}");
        if (end == NULL) {
            break;
        }
        body = xstrndup(after, (size_t)(end - after));
        trimmed = str_trim(body);
        if (str_starts_with(trimmed, "LANGUAGE")) {
            Vec parts = str_split(trimmed + 8, ',');
            size_t i;
            for (i = 0; i < parts.len; i++) {
                char *ext = str_trim((const char *)parts.items[i]);
                if (ext[0] != '\0') {
                    vec_push(&exts, ext);
                }
            }
        }
        rest = end + 3;
    }
    return exts;
}

LexOptions lex_options_from_source(const char *src)
{
    LexOptions opts;
    Vec exts = language_pragmas(src);
    size_t i;
    opts.magic_hash = 0;
    opts.unboxed = 0;
    for (i = 0; i < exts.len; i++) {
        const char *ext = (const char *)exts.items[i];
        if (str_eq(ext, "MagicHash")) {
            opts.magic_hash = 1;
        } else if (str_eq(ext, "UnboxedTuples") || str_eq(ext, "UnboxedSums")) {
            opts.unboxed = 1;
        }
    }
    return opts;
}

/* --- The lexer state ---------------------------------------------------- */

typedef struct {
    unsigned long *chars;
    size_t len;
    size_t idx;
    Pos pos;
    LexOptions opts;
    SyntaxError *err;
} Lexer;

#define NONE 0xFFFFFFFFUL

static int is_symbol(unsigned long c)
{
    switch (c) {
    case '!': case '#': case '$': case '%': case '&': case '*': case '+':
    case '.': case '/': case '<': case '=': case '>': case '?': case '@':
    case '\\': case '^': case '|': case '-': case '~': case ':':
        return 1;
    default:
        break;
    }
    if (c < 0x80) {
        return 0;
    }
    /* Unicode symbols and punctuation, but not letters or digits. */
    return !uc_is_alphanumeric(c) && !uc_is_whitespace(c) && !uc_is_control(c);
}

static int is_ident_start(unsigned long c)
{
    return c == '_' || uc_is_alphabetic(c);
}

static int is_ident_char(unsigned long c)
{
    return c == '_' || c == '\'' || uc_is_alphanumeric(c);
}

/* The value of digit c in the radix, or -1. */
static int digit_value(unsigned long c, unsigned radix)
{
    int d;
    if (c >= '0' && c <= '9') {
        d = (int)(c - '0');
    } else if (c >= 'a' && c <= 'z') {
        d = (int)(c - 'a') + 10;
    } else if (c >= 'A' && c <= 'Z') {
        d = (int)(c - 'A') + 10;
    } else {
        return -1;
    }
    return d < (int)radix ? d : -1;
}

static int is_digit(unsigned long c, unsigned radix)
{
    return c != NONE && c < 0x80 && digit_value(c, radix) >= 0;
}

static unsigned long peek_at(const Lexer *lx, size_t n)
{
    return lx->idx + n < lx->len ? lx->chars[lx->idx + n] : NONE;
}

static unsigned long peek(const Lexer *lx)
{
    return peek_at(lx, 0);
}

static int starts_with(const Lexer *lx, const char *s)
{
    size_t i;
    for (i = 0; s[i]; i++) {
        if (peek_at(lx, i) != (unsigned long)(unsigned char)s[i]) {
            return 0;
        }
    }
    return 1;
}

static unsigned long bump(Lexer *lx)
{
    unsigned long c = peek(lx);
    if (c == NONE) {
        return NONE;
    }
    lx->idx++;
    switch (c) {
    case '\n':
        lx->pos.line++;
        lx->pos.col = 1;
        break;
    case '\t':
        lx->pos.col = (lx->pos.col - 1) / 8 * 8 + 9;
        break;
    case '\r':
        break;
    default:
        lx->pos.col++;
        break;
    }
    return c;
}

static void bump_n(Lexer *lx, size_t n)
{
    size_t i;
    for (i = 0; i < n; i++) {
        bump(lx);
    }
}

/* Record an error. Returns -1, so callers can write `return error(...)`. */
static int error(Lexer *lx, Pos pos, char *message)
{
    lx->err->stage = "lex";
    lx->err->pos = pos;
    lx->err->message = message;
    return -1;
}

/* The source text of chars[from..to] as UTF-8. */
static char *slice(const Lexer *lx, size_t from, size_t to)
{
    StrBuf b;
    size_t i;
    sb_init(&b);
    for (i = from; i < to; i++) {
        sb_put_utf8(&b, lx->chars[i]);
    }
    return sb_take(&b);
}

static Token token(TokenKind kind, Pos pos)
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

/* --- Trivia ------------------------------------------------------------ */

/* `--` starts a line comment only when the dashes are not part of a
 * longer operator, so `-->` is an operator and `---` is a comment. */
static int is_line_comment(const Lexer *lx)
{
    size_t n = 0;
    unsigned long c;
    while (peek_at(lx, n) == '-') {
        n++;
    }
    c = peek_at(lx, n);
    return n >= 2 && !(c != NONE && is_symbol(c));
}

static int block_comment(Lexer *lx)
{
    Pos start = lx->pos;
    int depth = 1;
    bump_n(lx, 2);
    while (depth > 0) {
        if (starts_with(lx, "{-")) {
            bump_n(lx, 2);
            depth++;
        } else if (starts_with(lx, "-}")) {
            bump_n(lx, 2);
            depth--;
        } else if (bump(lx) == NONE) {
            return error(lx, start, xstrdup("unterminated block comment"));
        }
    }
    return 0;
}

/* A pragma. Sets *keep and fills *tok for the pragmas that matter. The
 * compiler ignores the others. */
static int pragma(Lexer *lx, Token *tok, int *keep)
{
    Pos start = lx->pos;
    StrBuf text;
    Vec words;
    sb_init(&text);
    bump_n(lx, 3);
    for (;;) {
        unsigned long c;
        if (starts_with(lx, "#-}")) {
            bump_n(lx, 3);
            break;
        }
        c = bump(lx);
        if (c == NONE) {
            return error(lx, start, xstrdup("unterminated pragma"));
        }
        sb_put_utf8(&text, c);
    }
    *tok = token(TOK_PRAGMA, start);
    tok->text = sb_take(&text);
    tok->end = lx->pos;
    words = str_split_ws(tok->text);
    *keep = words.len > 0 && str_eq((const char *)words.items[0], "LANGUAGE");
    return 0;
}

/* Skip white space and comments. Sets *found and fills *tok if a pragma
 * is found, because pragmas are the one kind of comment that stays. */
static int skip_trivia(Lexer *lx, Token *tok, int *found)
{
    *found = 0;
    for (;;) {
        unsigned long c = peek(lx);
        if (c == NONE) {
            return 0;
        }
        if (uc_is_whitespace(c)) {
            bump(lx);
        } else if (c == '-' && is_line_comment(lx)) {
            while ((c = peek(lx)) != NONE && c != '\n') {
                bump(lx);
            }
        } else if (c == '{' && starts_with(lx, "{-#")) {
            if (pragma(lx, tok, found) != 0) {
                return -1;
            }
            if (*found) {
                return 0;
            }
        } else if (c == '{' && starts_with(lx, "{-")) {
            if (block_comment(lx) != 0) {
                return -1;
            }
        } else {
            return 0;
        }
    }
}

/* --- Names and symbols ------------------------------------------------- */

/* An identifier segment: letters, digits, `_`, `'`, and with MagicHash
 * any number of trailing `#`. */
static char *ident(Lexer *lx)
{
    StrBuf s;
    unsigned long c;
    sb_init(&s);
    while ((c = peek(lx)) != NONE && is_ident_char(c)) {
        sb_put_utf8(&s, c);
        bump(lx);
    }
    if (lx->opts.magic_hash) {
        while (peek(lx) == '#') {
            sb_putc(&s, '#');
            bump(lx);
        }
    }
    return sb_take(&s);
}

static char *symbol(Lexer *lx)
{
    StrBuf s;
    unsigned long c;
    sb_init(&s);
    while ((c = peek(lx)) != NONE && is_symbol(c)) {
        sb_put_utf8(&s, c);
        bump(lx);
    }
    return sb_take(&s);
}

/* A name, possibly qualified: `x`, `Con`, `M.N.x`, `M.N.Con`, `M.+`,
 * `M.:+`. */
static void name(Lexer *lx, Token *tok)
{
    StrBuf qual;
    sb_init(&qual);
    for (;;) {
        char *seg = ident(lx);
        const char *p = seg;
        int is_con = seg[0] != '\0' && uc_is_uppercase(utf8_next(&p));
        if (is_con && peek(lx) == '.') {
            /* A qualifier, if a name follows the dot. */
            unsigned long c = peek_at(lx, 1);
            if (c != NONE && is_ident_start(c)) {
                bump(lx);
                if (qual.len > 0) {
                    sb_putc(&qual, '.');
                }
                sb_puts(&qual, seg);
                continue;
            }
            if (c != NONE && is_symbol(c)) {
                char *sym;
                bump(lx);
                if (qual.len > 0) {
                    sb_putc(&qual, '.');
                }
                sb_puts(&qual, seg);
                sym = symbol(lx);
                tok->kind = sym[0] == ':' ? TOK_CONSYM : TOK_VARSYM;
                tok->qual = sb_take(&qual);
                tok->text = sym;
                return;
            }
        }
        if (is_con) {
            tok->kind = TOK_CONID;
        } else if (qual.len == 0 && keyword_parse(seg) >= 0) {
            tok->kind = TOK_KEYWORD;
            tok->ch = (unsigned long)keyword_parse(seg);
            return;
        } else {
            tok->kind = TOK_VARID;
        }
        tok->qual = sb_take(&qual);
        tok->text = seg;
        return;
    }
}

/* --- Numbers ----------------------------------------------------------- */

static size_t digits(Lexer *lx, StrBuf *s, unsigned radix)
{
    size_t n = 0;
    unsigned long c;
    while ((c = peek(lx)) != NONE) {
        if (is_digit(c, radix)) {
            sb_put_utf8(s, c);
            bump(lx);
            n++;
        } else if (c == '_' && is_digit(peek_at(lx, 1), radix)) {
            /* NumericUnderscores */
            sb_put_utf8(s, c);
            bump(lx);
        } else {
            break;
        }
    }
    return n;
}

static void magic_hash_suffix(Lexer *lx, StrBuf *s)
{
    if (lx->opts.magic_hash) {
        while (peek(lx) == '#') {
            sb_putc(s, '#');
            bump(lx);
        }
    }
}

static int number(Lexer *lx, Token *tok)
{
    Pos start = lx->pos;
    StrBuf s;
    int is_float = 0;
    unsigned long c;
    sb_init(&s);
    if (peek(lx) == '0') {
        unsigned radix = 0;
        c = peek_at(lx, 1);
        if (c == 'x' || c == 'X') {
            radix = 16;
        } else if (c == 'o' || c == 'O') {
            radix = 8;
        } else if (c == 'b' || c == 'B') {
            radix = 2;
        }
        if (radix != 0 && is_digit(peek_at(lx, 2), radix)) {
            sb_put_utf8(&s, bump(lx));
            sb_put_utf8(&s, bump(lx));
            digits(lx, &s, radix);
            magic_hash_suffix(lx, &s);
            tok->kind = TOK_INTEGER;
            tok->text = sb_take(&s);
            return 0;
        }
    }
    digits(lx, &s, 10);
    if (peek(lx) == '.' && is_digit(peek_at(lx, 1), 10)) {
        is_float = 1;
        sb_putc(&s, '.');
        bump(lx);
        digits(lx, &s, 10);
    }
    c = peek(lx);
    if (c == 'e' || c == 'E') {
        unsigned long next = peek_at(lx, 1);
        int sign = next == '+' || next == '-';
        size_t digit_at = sign ? 2 : 1;
        if (is_digit(peek_at(lx, digit_at), 10)) {
            is_float = 1;
            sb_put_utf8(&s, bump(lx));
            if (sign) {
                sb_put_utf8(&s, bump(lx));
            }
            if (digits(lx, &s, 10) == 0) {
                return error(lx, start, xstrdup("malformed exponent"));
            }
        }
    }
    magic_hash_suffix(lx, &s);
    tok->kind = is_float ? TOK_FLOAT : TOK_INTEGER;
    tok->text = sb_take(&s);
    return 0;
}

/* --- Characters and strings -------------------------------------------- */

/* The named ASCII escapes of the Haskell report. */
static const struct {
    const char *name;
    unsigned long ch;
} ascii_names[] = {
    {"NUL", 0x00}, {"SOH", 0x01}, {"STX", 0x02}, {"ETX", 0x03},
    {"EOT", 0x04}, {"ENQ", 0x05}, {"ACK", 0x06}, {"BEL", 0x07},
    {"BS", 0x08},  {"HT", 0x09},  {"LF", 0x0a},  {"VT", 0x0b},
    {"FF", 0x0c},  {"CR", 0x0d},  {"SO", 0x0e},  {"SI", 0x0f},
    {"DLE", 0x10}, {"DC1", 0x11}, {"DC2", 0x12}, {"DC3", 0x13},
    {"DC4", 0x14}, {"NAK", 0x15}, {"SYN", 0x16}, {"ETB", 0x17},
    {"CAN", 0x18}, {"EM", 0x19},  {"SUB", 0x1a}, {"ESC", 0x1b},
    {"FS", 0x1c},  {"GS", 0x1d},  {"RS", 0x1e},  {"US", 0x1f},
    {"SP", 0x20},  {"DEL", 0x7f}
};

/* A code point in the form of Rust's `{c:?}` for an error message. */
static char *debug_char(unsigned long c)
{
    if (c == '\'') {
        return xstrdup("'\\''");
    }
    if (c == '\\') {
        return xstrdup("'\\\\'");
    }
    if (c == '\n') {
        return xstrdup("'\\n'");
    }
    if (c == '\t') {
        return xstrdup("'\\t'");
    }
    if (c == '\r') {
        return xstrdup("'\\r'");
    }
    if (c == 0) {
        return xstrdup("'\\0'");
    }
    if (uc_is_control(c)) {
        StrBuf b;
        const char *hex = "0123456789abcdef";
        int shift, started = 0;
        sb_init(&b);
        sb_puts(&b, "'\\u{");
        for (shift = 20; shift >= 0; shift -= 4) {
            unsigned d = (unsigned)((c >> shift) & 0xF);
            if (d || started || shift == 0) {
                sb_putc(&b, hex[d]);
                started = 1;
            }
        }
        sb_puts(&b, "}'");
        return sb_take(&b);
    }
    return str_printf("'%U'", c);
}

static int numeric_escape(Lexer *lx, Pos start, unsigned radix, unsigned long *out)
{
    unsigned long value = 0;
    size_t n = 0;
    unsigned long c;
    while ((c = peek(lx)) != NONE) {
        int d = c < 0x80 ? digit_value(c, radix) : -1;
        if (d < 0) {
            break;
        }
        if (value > (0xFFFFFFFFUL - (unsigned long)d) / radix) {
            return error(lx, start, xstrdup("numeric escape out of range"));
        }
        value = value * radix + (unsigned long)d;
        bump(lx);
        n++;
    }
    if (n == 0) {
        return error(lx, start, xstrdup("numeric escape without digits"));
    }
    if (value > 0x10FFFF || (value >= 0xD800 && value <= 0xDFFF)) {
        return error(lx, start, xstrdup("numeric escape out of range"));
    }
    *out = value;
    return 0;
}

/* An escape after the backslash. Sets *out to the character, or to NONE
 * for `\&`, the empty escape. */
static int escape(Lexer *lx, Pos start, unsigned long *out)
{
    unsigned long c = bump(lx);
    if (c == NONE) {
        return error(lx, start, xstrdup("unterminated escape"));
    }
    switch (c) {
    case 'a': *out = 0x07; return 0;
    case 'b': *out = 0x08; return 0;
    case 'f': *out = 0x0c; return 0;
    case 'n': *out = '\n'; return 0;
    case 'r': *out = '\r'; return 0;
    case 't': *out = '\t'; return 0;
    case 'v': *out = 0x0b; return 0;
    case '\\': *out = '\\'; return 0;
    case '"': *out = '"'; return 0;
    case '\'': *out = '\''; return 0;
    case '&': *out = NONE; return 0;
    case '^':
        c = bump(lx);
        if (c == NONE) {
            return error(lx, start, xstrdup("unterminated escape"));
        }
        if (c >= '@' && c <= '_') {
            *out = c - '@';
            return 0;
        }
        return error(lx, start, str_printf("bad control escape \\^%U", c));
    case 'x':
        return numeric_escape(lx, start, 16, out);
    case 'o':
        return numeric_escape(lx, start, 8, out);
    default:
        break;
    }
    if (c >= '0' && c <= '9') {
        lx->idx--;
        lx->pos.col--;
        return numeric_escape(lx, start, 10, out);
    }
    if (c >= 'A' && c <= 'Z') {
        /* A named ASCII control character, such as `\NUL`. The lexer
         * matches the longest name, so `\SOH` is one escape and not
         * `\SO` followed by `H`. */
        size_t i, best = 0, best_len = 0;
        lx->idx--;
        lx->pos.col--;
        for (i = 0; i < sizeof ascii_names / sizeof ascii_names[0]; i++) {
            size_t n = strlen(ascii_names[i].name);
            if (starts_with(lx, ascii_names[i].name) && n > best_len) {
                best = i;
                best_len = n;
            }
        }
        if (best_len == 0) {
            return error(lx, start, str_printf("unknown escape \\%U", c));
        }
        bump_n(lx, best_len);
        *out = ascii_names[best].ch;
        return 0;
    }
    return error(lx, start, str_printf("unknown escape \\%U", c));
}

static int char_or_tick(Lexer *lx, Token *tok)
{
    Pos start = lx->pos;
    /* Look ahead without consuming: a char literal is `'c'` or
     * `'\escape'`. Anything else is a tick. */
    size_t save_idx = lx->idx;
    Pos save_pos = lx->pos;
    unsigned long c = NONE, next;
    bump(lx);
    next = peek(lx);
    if (next == '\\') {
        bump(lx);
        if (escape(lx, start, &c) != 0) {
            return -1;
        }
        if (c == NONE) {
            return error(lx, start, xstrdup("\\& is not permitted in a character literal"));
        }
    } else if (next != NONE && next != '\'' && next != '\n') {
        bump(lx);
        c = next;
    }
    if (c != NONE && peek(lx) == '\'') {
        StrBuf hash;
        bump(lx);
        /* A primitive character literal: 'c'# (MagicHash). */
        sb_init(&hash);
        magic_hash_suffix(lx, &hash);
        tok->kind = TOK_CHAR;
        tok->ch = c;
        tok->text = slice(lx, save_idx, lx->idx);
        return 0;
    }
    lx->idx = save_idx;
    lx->pos = save_pos;
    bump(lx);
    tok->kind = TOK_TICK;
    return 0;
}

static int string(Lexer *lx, Token *tok)
{
    Pos start = lx->pos;
    size_t start_idx = lx->idx;
    StrBuf s;
    sb_init(&s);
    bump(lx);
    for (;;) {
        unsigned long c = bump(lx);
        if (c == NONE || c == '\n') {
            return error(lx, start, xstrdup("unterminated string literal"));
        }
        if (c == '"') {
            /* A primitive string literal: "s"# (MagicHash). */
            StrBuf hash;
            sb_init(&hash);
            magic_hash_suffix(lx, &hash);
            tok->kind = TOK_STRING;
            tok->value_len = s.len;
            tok->value = sb_take(&s);
            tok->text = slice(lx, start_idx, lx->idx);
            return 0;
        }
        if (c == '\\') {
            unsigned long next = peek(lx);
            if (next != NONE && uc_is_whitespace(next)) {
                /* A string gap: `\` white space `\`. */
                while ((next = peek(lx)) != NONE && uc_is_whitespace(next)) {
                    bump(lx);
                }
                if (bump(lx) != '\\') {
                    return error(lx, start, xstrdup("malformed string gap"));
                }
            } else {
                unsigned long e;
                if (escape(lx, start, &e) != 0) {
                    return -1;
                }
                if (e != NONE) {
                    sb_put_utf8(&s, e);
                }
            }
        } else {
            sb_put_utf8(&s, c);
        }
    }
}

/* --- Tokens ------------------------------------------------------------ */

static int next_token(Lexer *lx, Token *tok)
{
    Pos start;
    unsigned long c;
    int found;
    if (skip_trivia(lx, tok, &found) != 0) {
        return -1;
    }
    if (found) {
        return 0;
    }
    start = lx->pos;
    *tok = token(TOK_EOF, start);
    c = peek(lx);
    if (c == NONE) {
        return 0;
    }
    /* UnboxedTuples: `(#` opens and `#)` closes. `(#)` stays the
     * operator `#` in parentheses. */
    if (lx->opts.unboxed && c == '(' && peek_at(lx, 1) == '#' && peek_at(lx, 2) != ')') {
        bump_n(lx, 2);
        tok->kind = TOK_SPECIAL;
        tok->ch = SPECIAL_UOPEN;
        tok->end = lx->pos;
        return 0;
    }
    if (lx->opts.unboxed && c == '#' && peek_at(lx, 1) == ')') {
        bump_n(lx, 2);
        tok->kind = TOK_SPECIAL;
        tok->ch = SPECIAL_UCLOSE;
        tok->end = lx->pos;
        return 0;
    }
    switch (c) {
    case '(': case ')': case ',': case ';': case '[': case ']': case '`':
    case '{': case '}':
        bump(lx);
        tok->kind = TOK_SPECIAL;
        tok->ch = c;
        break;
    case '"':
        if (string(lx, tok) != 0) {
            return -1;
        }
        break;
    case '\'':
        if (char_or_tick(lx, tok) != 0) {
            return -1;
        }
        break;
    default:
        if (c >= '0' && c <= '9') {
            if (number(lx, tok) != 0) {
                return -1;
            }
        } else if (is_ident_start(c)) {
            name(lx, tok);
        } else if (is_symbol(c)) {
            char *sym = symbol(lx);
            int op = reservedop_parse(sym);
            if (op >= 0) {
                tok->kind = TOK_RESERVEDOP;
                tok->ch = (unsigned long)op;
            } else {
                tok->kind = sym[0] == ':' ? TOK_CONSYM : TOK_VARSYM;
                tok->text = sym;
            }
        } else {
            return error(lx, start, str_printf("unexpected character %s", debug_char(c)));
        }
        break;
    }
    tok->end = lx->pos;
    return 0;
}

int lex(const char *src, size_t len, LexOptions opts, TokenVec *out, SyntaxError *err)
{
    Lexer lx;
    lx.len = utf8_decode(src, len, &lx.chars);
    lx.idx = 0;
    lx.pos.line = 1;
    lx.pos.col = 1;
    lx.opts = opts;
    lx.err = err;
    out->items = NULL;
    out->len = 0;
    out->cap = 0;
    for (;;) {
        Token tok;
        if (next_token(&lx, &tok) != 0) {
            return -1;
        }
        tokvec_push(out, tok);
        if (tok.kind == TOK_EOF) {
            return 0;
        }
    }
}

int tokenize(const char *src, size_t len, TokenVec *out, SyntaxError *err)
{
    TokenVec raw;
    if (lex(src, len, lex_options_from_source(src), &raw, err) != 0) {
        return -1;
    }
    return layout(&raw, out, err);
}
