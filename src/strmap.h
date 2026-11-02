/* A map from strings to pointers, sorted by key in byte order.
 *
 * The map keeps its keys in a sorted array and finds them with a binary
 * search. Iteration in index order gives the keys in byte order, as for
 * Rust's BTreeMap<String, _>.
 */
#ifndef AIHC_STRMAP_H
#define AIHC_STRMAP_H

#include <stddef.h>

typedef struct {
    char **keys;
    void **vals;
    size_t len;
    size_t cap;
} StrMap;

void strmap_init(StrMap *m);
/* The value for key, or NULL when the map does not have the key. */
void *strmap_get(const StrMap *m, const char *key);
/* Whether the map has key. */
int strmap_has(const StrMap *m, const char *key);
/* Set the value for key. A value that is in the map already is replaced.
 * The map keeps the key pointer. */
void strmap_put(StrMap *m, const char *key, void *val);
/* The value for key. When the map does not have the key, insert val and
 * return it. */
void *strmap_get_or_put(StrMap *m, const char *key, void *val);
/* Remove key, when the map has it. */
void strmap_remove(StrMap *m, const char *key);
/* Add the entries of other whose keys this map does not have. The values
 * are shared, not copied. */
void strmap_union(StrMap *m, const StrMap *other);
/* A copy of the map. The keys and values are shared. */
StrMap strmap_copy(const StrMap *m);

#endif
