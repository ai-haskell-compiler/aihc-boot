/* Utilities: memory, growable arrays, string builders and Unicode. */

#include "util.h"
#include "unicode_tables.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* --- Memory ------------------------------------------------------------ */

void *xmalloc(size_t n)
{
    void *p = malloc(n ? n : 1);
    if (p == NULL) {
        die("out of memory");
    }
    return p;
}

void *xcalloc(size_t n, size_t size)
{
    void *p = calloc(n ? n : 1, size ? size : 1);
    if (p == NULL) {
        die("out of memory");
    }
    return p;
}

void *xrealloc(void *p, size_t n)
{
    p = realloc(p, n ? n : 1);
    if (p == NULL) {
        die("out of memory");
    }
    return p;
}

char *xstrdup(const char *s)
{
    return xstrndup(s, strlen(s));
}

char *xstrndup(const char *s, size_t n)
{
    char *p = (char *)xmalloc(n + 1);
    memcpy(p, s, n);
    p[n] = '\0';
    return p;
}

/* --- Growable arrays --------------------------------------------------- */

void vec_init(Vec *v)
{
    v->items = NULL;
    v->len = 0;
    v->cap = 0;
}

static void vec_reserve(Vec *v, size_t n)
{
    if (n > v->cap) {
        size_t cap = v->cap ? v->cap * 2 : 8;
        while (cap < n) {
            cap *= 2;
        }
        v->items = (void **)xrealloc(v->items, cap * sizeof(void *));
        v->cap = cap;
    }
}

void vec_push(Vec *v, void *x)
{
    vec_reserve(v, v->len + 1);
    v->items[v->len++] = x;
}

void *vec_pop(Vec *v)
{
    if (v->len == 0) {
        return NULL;
    }
    return v->items[--v->len];
}

void *vec_last(const Vec *v)
{
    return v->len ? v->items[v->len - 1] : NULL;
}

void vec_remove(Vec *v, size_t i)
{
    if (i >= v->len) {
        return;
    }
    memmove(v->items + i, v->items + i + 1, (v->len - i - 1) * sizeof(void *));
    v->len--;
}

void vec_insert(Vec *v, size_t i, void *x)
{
    vec_reserve(v, v->len + 1);
    memmove(v->items + i + 1, v->items + i, (v->len - i) * sizeof(void *));
    v->items[i] = x;
    v->len++;
}

void vec_sort(Vec *v, int (*cmp)(const void *, const void *))
{
    if (v->len > 1) {
        qsort(v->items, v->len, sizeof(void *), cmp);
    }
}

int vec_has_str(const Vec *v, const char *s)
{
    size_t i;
    for (i = 0; i < v->len; i++) {
        if (strcmp((const char *)v->items[i], s) == 0) {
            return 1;
        }
    }
    return 0;
}

/* --- String builder ---------------------------------------------------- */

void sb_init(StrBuf *b)
{
    b->s = NULL;
    b->len = 0;
    b->cap = 0;
}

void sb_clear(StrBuf *b)
{
    b->len = 0;
    if (b->s) {
        b->s[0] = '\0';
    }
}

static void sb_reserve(StrBuf *b, size_t extra)
{
    size_t need = b->len + extra + 1;
    if (need > b->cap) {
        size_t cap = b->cap ? b->cap * 2 : 32;
        while (cap < need) {
            cap *= 2;
        }
        b->s = (char *)xrealloc(b->s, cap);
        b->cap = cap;
    }
}

void sb_putn(StrBuf *b, const char *s, size_t n)
{
    sb_reserve(b, n);
    memcpy(b->s + b->len, s, n);
    b->len += n;
    b->s[b->len] = '\0';
}

void sb_putc(StrBuf *b, int c)
{
    char ch = (char)c;
    sb_putn(b, &ch, 1);
}

void sb_puts(StrBuf *b, const char *s)
{
    sb_putn(b, s, strlen(s));
}

void sb_putul(StrBuf *b, unsigned long n)
{
    char buf[32];
    int i = (int)sizeof buf;
    do {
        buf[--i] = (char)('0' + n % 10);
        n /= 10;
    } while (n);
    sb_putn(b, buf + i, sizeof buf - (size_t)i);
}

void sb_putl(StrBuf *b, long n)
{
    if (n < 0) {
        sb_putc(b, '-');
        sb_putul(b, (unsigned long)(-(n + 1)) + 1);
    } else {
        sb_putul(b, (unsigned long)n);
    }
}

void sb_put_utf8(StrBuf *b, unsigned long c)
{
    char buf[4];
    if (c < 0x80) {
        buf[0] = (char)c;
        sb_putn(b, buf, 1);
    } else if (c < 0x800) {
        buf[0] = (char)(0xC0 | (c >> 6));
        buf[1] = (char)(0x80 | (c & 0x3F));
        sb_putn(b, buf, 2);
    } else if (c < 0x10000) {
        buf[0] = (char)(0xE0 | (c >> 12));
        buf[1] = (char)(0x80 | ((c >> 6) & 0x3F));
        buf[2] = (char)(0x80 | (c & 0x3F));
        sb_putn(b, buf, 3);
    } else {
        buf[0] = (char)(0xF0 | (c >> 18));
        buf[1] = (char)(0x80 | ((c >> 12) & 0x3F));
        buf[2] = (char)(0x80 | ((c >> 6) & 0x3F));
        buf[3] = (char)(0x80 | (c & 0x3F));
        sb_putn(b, buf, 4);
    }
}

static void sb_vprintf(StrBuf *b, const char *fmt, va_list ap)
{
    const char *p;
    for (p = fmt; *p; p++) {
        if (*p != '%') {
            sb_putc(b, *p);
            continue;
        }
        p++;
        switch (*p) {
        case 's':
            sb_puts(b, va_arg(ap, const char *));
            break;
        case 'd':
            sb_putl(b, (long)va_arg(ap, int));
            break;
        case 'c':
            sb_putc(b, va_arg(ap, int));
            break;
        case 'U':
            sb_put_utf8(b, va_arg(ap, unsigned long));
            break;
        case 'l':
            p++;
            if (*p == 'u') {
                sb_putul(b, va_arg(ap, unsigned long));
            } else {
                sb_putl(b, va_arg(ap, long));
            }
            break;
        case '%':
            sb_putc(b, '%');
            break;
        case '\0':
            return;
        default:
            sb_putc(b, '%');
            sb_putc(b, *p);
            break;
        }
    }
}

void sb_printf(StrBuf *b, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    sb_vprintf(b, fmt, ap);
    va_end(ap);
}

char *sb_dup(const StrBuf *b)
{
    return b->s ? xstrndup(b->s, b->len) : xstrdup("");
}

char *sb_take(StrBuf *b)
{
    char *s = b->s ? b->s : xstrdup("");
    sb_init(b);
    return s;
}

char *str_printf(const char *fmt, ...)
{
    StrBuf b;
    va_list ap;
    sb_init(&b);
    va_start(ap, fmt);
    sb_vprintf(&b, fmt, ap);
    va_end(ap);
    return sb_take(&b);
}

/* --- Strings ----------------------------------------------------------- */

int str_eq(const char *a, const char *b)
{
    return strcmp(a, b) == 0;
}

int str_starts_with(const char *s, const char *prefix)
{
    return strncmp(s, prefix, strlen(prefix)) == 0;
}

int str_ends_with(const char *s, const char *suffix)
{
    size_t n = strlen(s), m = strlen(suffix);
    return n >= m && strcmp(s + n - m, suffix) == 0;
}

/* The length of the white space character at s, or 0. */
static size_t ws_len(const char *s)
{
    const char *p = s;
    unsigned long c;
    if (*s == '\0') {
        return 0;
    }
    c = utf8_next(&p);
    return uc_is_whitespace(c) ? (size_t)(p - s) : 0;
}

char *str_trim(const char *s)
{
    const char *start = s, *end, *p;
    size_t n;
    while ((n = ws_len(start)) > 0) {
        start += n;
    }
    end = start;
    p = start;
    while (*p) {
        n = ws_len(p);
        if (n > 0) {
            p += n;
        } else {
            utf8_next(&p);
            end = p;
        }
    }
    return xstrndup(start, (size_t)(end - start));
}

Vec str_split(const char *s, int sep)
{
    Vec v;
    const char *start = s, *p;
    vec_init(&v);
    for (p = s;; p++) {
        if (*p == (char)sep || *p == '\0') {
            vec_push(&v, xstrndup(start, (size_t)(p - start)));
            if (*p == '\0') {
                break;
            }
            start = p + 1;
        }
    }
    return v;
}

Vec str_split_ws(const char *s)
{
    Vec v;
    const char *p = s, *start;
    size_t n;
    vec_init(&v);
    while (*p) {
        while ((n = ws_len(p)) > 0) {
            p += n;
        }
        if (*p == '\0') {
            break;
        }
        start = p;
        while (*p && ws_len(p) == 0) {
            utf8_next(&p);
        }
        vec_push(&v, xstrndup(start, (size_t)(p - start)));
    }
    return v;
}

Vec str_lines(const char *s)
{
    Vec v;
    const char *start = s, *p;
    vec_init(&v);
    for (p = s; *p; p++) {
        if (*p == '\n') {
            size_t n = (size_t)(p - start);
            if (n > 0 && start[n - 1] == '\r') {
                n--;
            }
            vec_push(&v, xstrndup(start, n));
            start = p + 1;
        }
    }
    if (*start) {
        size_t n = strlen(start);
        if (n > 0 && start[n - 1] == '\r') {
            n--;
        }
        vec_push(&v, xstrndup(start, n));
    }
    return v;
}

char *str_concat(const char *a, const char *b)
{
    return str_concat3(a, b, "");
}

char *str_concat3(const char *a, const char *b, const char *c)
{
    size_t na = strlen(a), nb = strlen(b), nc = strlen(c);
    char *p = (char *)xmalloc(na + nb + nc + 1);
    memcpy(p, a, na);
    memcpy(p + na, b, nb);
    memcpy(p + na + nb, c, nc + 1);
    return p;
}

/* --- Unicode ----------------------------------------------------------- */

/* Decode one code point from s (at most n bytes). Sets *used. Invalid
 * input gives U+FFFD. */
static unsigned long utf8_one(const unsigned char *s, size_t n, size_t *used)
{
    unsigned long c;
    size_t len, i;
    unsigned long min;
    if (s[0] < 0x80) {
        *used = 1;
        return s[0];
    }
    if ((s[0] & 0xE0) == 0xC0) {
        len = 2;
        c = s[0] & 0x1F;
        min = 0x80;
    } else if ((s[0] & 0xF0) == 0xE0) {
        len = 3;
        c = s[0] & 0x0F;
        min = 0x800;
    } else if ((s[0] & 0xF8) == 0xF0) {
        len = 4;
        c = s[0] & 0x07;
        min = 0x10000;
    } else {
        *used = 1;
        return 0xFFFD;
    }
    for (i = 1; i < len; i++) {
        if (i >= n || (s[i] & 0xC0) != 0x80) {
            *used = i;
            return 0xFFFD;
        }
        c = (c << 6) | (s[i] & 0x3F);
    }
    *used = len;
    if (c < min || c > 0x10FFFF || (c >= 0xD800 && c <= 0xDFFF)) {
        return 0xFFFD;
    }
    return c;
}

size_t utf8_decode(const char *s, size_t n, unsigned long **out)
{
    unsigned long *buf = (unsigned long *)xmalloc((n + 1) * sizeof(unsigned long));
    size_t i = 0, k = 0, used;
    while (i < n) {
        buf[k++] = utf8_one((const unsigned char *)s + i, n - i, &used);
        i += used;
    }
    *out = buf;
    return k;
}

unsigned long utf8_next(const char **p)
{
    size_t used;
    size_t n = strlen(*p);
    unsigned long c;
    if (n > 4) {
        n = 4;
    }
    c = utf8_one((const unsigned char *)*p, n, &used);
    *p += used;
    return c;
}

static int in_table(unsigned long c, const unsigned long (*t)[2], unsigned long len)
{
    unsigned long lo = 0, hi = len;
    while (lo < hi) {
        unsigned long mid = lo + (hi - lo) / 2;
        if (c < t[mid][0]) {
            hi = mid;
        } else if (c > t[mid][1]) {
            lo = mid + 1;
        } else {
            return 1;
        }
    }
    return 0;
}

int uc_is_whitespace(unsigned long c)
{
    if (c < 0x80) {
        return c == ' ' || (c >= 9 && c <= 13);
    }
    return c == 0x85 || c == 0xA0 || c == 0x1680 || (c >= 0x2000 && c <= 0x200A) ||
           c == 0x2028 || c == 0x2029 || c == 0x202F || c == 0x205F || c == 0x3000;
}

int uc_is_alphabetic(unsigned long c)
{
    if (c < 0x80) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    }
    return in_table(c, uc_alpha, uc_alpha_len);
}

int uc_is_alphanumeric(unsigned long c)
{
    if (c < 0x80) {
        return uc_is_alphabetic(c) || (c >= '0' && c <= '9');
    }
    return in_table(c, uc_alpha, uc_alpha_len) || in_table(c, uc_numeric, uc_numeric_len);
}

int uc_is_uppercase(unsigned long c)
{
    if (c < 0x80) {
        return c >= 'A' && c <= 'Z';
    }
    return in_table(c, uc_upper, uc_upper_len);
}

int uc_general_category(unsigned long c)
{
    unsigned long lo = 0;
    unsigned long hi = uc_category_len;
    while (lo < hi) {
        unsigned long mid = lo + (hi - lo) / 2;
        if (c < uc_category[mid][0]) {
            hi = mid;
        } else if (c > uc_category[mid][1]) {
            lo = mid + 1;
        } else {
            return (int)uc_category[mid][2];
        }
    }
    return 29;
}

int uc_is_control(unsigned long c)
{
    return c < 0x20 || (c >= 0x7F && c <= 0x9F);
}

/* --- Files ------------------------------------------------------------- */

char *read_file(const char *path, size_t *len)
{
    FILE *f = fopen(path, "rb");
    StrBuf b;
    char buf[65536];
    size_t n;
    if (f == NULL) {
        return NULL;
    }
    sb_init(&b);
    while ((n = fread(buf, 1, sizeof buf, f)) > 0) {
        sb_putn(&b, buf, n);
    }
    if (ferror(f)) {
        fclose(f);
        free(b.s);
        return NULL;
    }
    fclose(f);
    if (len) {
        *len = b.len;
    }
    return sb_take(&b);
}

int write_file(const char *path, const char *data, size_t len)
{
    FILE *f = fopen(path, "wb");
    if (f == NULL) {
        return -1;
    }
    if (fwrite(data, 1, len, f) != len) {
        fclose(f);
        return -1;
    }
    return fclose(f) == 0 ? 0 : -1;
}

void die(const char *fmt, ...)
{
    StrBuf b;
    va_list ap;
    sb_init(&b);
    va_start(ap, fmt);
    sb_vprintf(&b, fmt, ap);
    va_end(ap);
    fprintf(stderr, "aihc-boot: %s\n", b.s ? b.s : "");
    exit(1);
}
