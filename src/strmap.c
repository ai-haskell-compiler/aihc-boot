/* A map from strings to pointers, sorted by key in byte order. */
#include <stdlib.h>
#include <string.h>

#include "strmap.h"
#include "util.h"

void strmap_init(StrMap *m)
{
    m->keys = NULL;
    m->vals = NULL;
    m->len = 0;
    m->cap = 0;
}

/* The index of key, or the index where key goes. Sets *found. */
static size_t find(const StrMap *m, const char *key, int *found)
{
    size_t lo = 0;
    size_t hi = m->len;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        int c = strcmp(m->keys[mid], key);
        if (c == 0) {
            *found = 1;
            return mid;
        }
        if (c < 0) {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    *found = 0;
    return lo;
}

static void reserve(StrMap *m, size_t n)
{
    if (n <= m->cap) {
        return;
    }
    if (m->cap == 0) {
        m->cap = 8;
    }
    while (m->cap < n) {
        m->cap *= 2;
    }
    m->keys = (char **)xrealloc(m->keys, m->cap * sizeof *m->keys);
    m->vals = (void **)xrealloc(m->vals, m->cap * sizeof *m->vals);
}

static void insert_at(StrMap *m, size_t i, const char *key, void *val)
{
    reserve(m, m->len + 1);
    memmove(m->keys + i + 1, m->keys + i, (m->len - i) * sizeof *m->keys);
    memmove(m->vals + i + 1, m->vals + i, (m->len - i) * sizeof *m->vals);
    m->keys[i] = (char *)key;
    m->vals[i] = val;
    m->len++;
}

void *strmap_get(const StrMap *m, const char *key)
{
    int found;
    size_t i = find(m, key, &found);
    return found ? m->vals[i] : NULL;
}

int strmap_has(const StrMap *m, const char *key)
{
    int found;
    find(m, key, &found);
    return found;
}

void strmap_put(StrMap *m, const char *key, void *val)
{
    int found;
    size_t i = find(m, key, &found);
    if (found) {
        m->vals[i] = val;
    } else {
        insert_at(m, i, key, val);
    }
}

void *strmap_get_or_put(StrMap *m, const char *key, void *val)
{
    int found;
    size_t i = find(m, key, &found);
    if (found) {
        return m->vals[i];
    }
    insert_at(m, i, key, val);
    return val;
}

void strmap_remove(StrMap *m, const char *key)
{
    int found;
    size_t i = find(m, key, &found);
    if (!found) {
        return;
    }
    memmove(m->keys + i, m->keys + i + 1, (m->len - i - 1) * sizeof *m->keys);
    memmove(m->vals + i, m->vals + i + 1, (m->len - i - 1) * sizeof *m->vals);
    m->len--;
}

void strmap_union(StrMap *m, const StrMap *other)
{
    /* Merge the two sorted arrays into new arrays. The entries of m win
     * over the entries of other with the same key. */
    char **keys;
    void **vals;
    size_t i = 0;
    size_t j = 0;
    size_t n = 0;
    size_t cap;
    if (other->len == 0) {
        return;
    }
    cap = m->len + other->len;
    keys = (char **)xmalloc(cap * sizeof *keys);
    vals = (void **)xmalloc(cap * sizeof *vals);
    while (i < m->len || j < other->len) {
        int c;
        if (i == m->len) {
            c = 1;
        } else if (j == other->len) {
            c = -1;
        } else {
            c = strcmp(m->keys[i], other->keys[j]);
        }
        if (c <= 0) {
            keys[n] = m->keys[i];
            vals[n] = m->vals[i];
            n++;
            i++;
            if (c == 0) {
                j++;
            }
        } else {
            keys[n] = other->keys[j];
            vals[n] = other->vals[j];
            n++;
            j++;
        }
    }
    free(m->keys);
    free(m->vals);
    m->keys = keys;
    m->vals = vals;
    m->len = n;
    m->cap = cap;
}

StrMap strmap_copy(const StrMap *m)
{
    StrMap out;
    strmap_init(&out);
    reserve(&out, m->len);
    if (m->len > 0) {
        memcpy(out.keys, m->keys, m->len * sizeof *m->keys);
        memcpy(out.vals, m->vals, m->len * sizeof *m->vals);
    }
    out.len = m->len;
    return out;
}
