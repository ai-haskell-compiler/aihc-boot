/* Utilities: memory, growable arrays, string builders and Unicode.
 *
 * aihc-boot is a short-lived process. It allocates with xmalloc and
 * does not free most memory. The operating system takes it back at exit.
 */
#ifndef AIHC_UTIL_H
#define AIHC_UTIL_H

#include <stddef.h>

/* --- Memory ------------------------------------------------------------ */

void *xmalloc(size_t n);
void *xcalloc(size_t n, size_t size);
void *xrealloc(void *p, size_t n);
char *xstrdup(const char *s);
/* A copy of the first n bytes of s, with a NUL at the end. */
char *xstrndup(const char *s, size_t n);

/* --- Growable arrays of pointers ---------------------------------------- */

typedef struct {
    void **items;
    size_t len;
    size_t cap;
} Vec;

void vec_init(Vec *v);
void vec_push(Vec *v, void *x);
void *vec_pop(Vec *v);
void *vec_last(const Vec *v);
/* Remove item i and move the items after it down by one. */
void vec_remove(Vec *v, size_t i);
void vec_insert(Vec *v, size_t i, void *x);
/* Sort with qsort; cmp receives pointers to the item pointers. */
void vec_sort(Vec *v, int (*cmp)(const void *, const void *));
/* Whether the vector holds a string equal to s (items are char *). */
int vec_has_str(const Vec *v, const char *s);

/* --- String builder ---------------------------------------------------- */

typedef struct {
    char *s;
    size_t len;
    size_t cap;
} StrBuf;

void sb_init(StrBuf *b);
void sb_clear(StrBuf *b);
void sb_putc(StrBuf *b, int c);
void sb_puts(StrBuf *b, const char *s);
void sb_putn(StrBuf *b, const char *s, size_t n);
void sb_putul(StrBuf *b, unsigned long n);
void sb_putl(StrBuf *b, long n);
/* Append the UTF-8 encoding of code point c. */
void sb_put_utf8(StrBuf *b, unsigned long c);
/* A small printf: %s, %d, %ld, %lu, %c (a byte), %U (a code point as
 * UTF-8, unsigned long) and %%. */
void sb_printf(StrBuf *b, const char *fmt, ...);
/* The contents as a fresh NUL-terminated string. The buffer stays valid. */
char *sb_dup(const StrBuf *b);
/* The contents; the buffer gives up ownership. Never NULL. */
char *sb_take(StrBuf *b);

/* A fresh string from a format, as for sb_printf. */
char *str_printf(const char *fmt, ...);

/* --- Strings ----------------------------------------------------------- */

int str_eq(const char *a, const char *b);
int str_starts_with(const char *s, const char *prefix);
int str_ends_with(const char *s, const char *suffix);
/* A copy without leading and trailing white space (Unicode white space,
 * as Rust's str::trim). */
char *str_trim(const char *s);
/* Split s at sep. Returns a Vec of fresh strings (the pieces are not
 * trimmed; empty pieces stay). */
Vec str_split(const char *s, int sep);
/* Split s at runs of white space, as Rust's split_whitespace. */
Vec str_split_ws(const char *s);
/* Split s into lines, as Rust's str::lines: "\n" or "\r\n" ends a line,
 * and a final line without "\n" counts. */
Vec str_lines(const char *s);
char *str_concat(const char *a, const char *b);
char *str_concat3(const char *a, const char *b, const char *c);

/* --- Unicode ----------------------------------------------------------- */

/* Decode UTF-8 into code points. Invalid bytes become U+FFFD, as Rust's
 * String::from_utf8_lossy. Returns the number of code points and sets
 * *out to a fresh array. */
size_t utf8_decode(const char *s, size_t n, unsigned long **out);
/* Decode the code point at *p and move *p past it. */
unsigned long utf8_next(const char **p);

/* Character classes, as the Rust char methods of the same names. Outside
 * ASCII they are approximations that cover the scripts in use. */
int uc_is_whitespace(unsigned long c);
int uc_is_alphabetic(unsigned long c);
int uc_is_alphanumeric(unsigned long c);
int uc_is_uppercase(unsigned long c);
int uc_is_control(unsigned long c);

/* --- Files ------------------------------------------------------------- */

/* Read a whole file. Returns NULL on failure. Sets *len if len is not
 * NULL. The result has a NUL at the end. */
char *read_file(const char *path, size_t *len);
/* Write a whole file. Returns 0 on success. */
int write_file(const char *path, const char *data, size_t len);

/* Print a message to stderr and exit with status 1. */
void die(const char *fmt, ...);

#endif
