/* The comparison of two resolutions of one module. See resolve_compare in
 * resolve.h for the rules. */
#include <stdlib.h>
#include <string.h>

#include "resolve.h"

/* A map from local ids to local ids, as a sorted array. */
typedef struct {
    unsigned long *keys;
    unsigned long *vals;
    size_t len;
    size_t cap;
} IdMap;

/* The value for key. When the map does not have the key, insert val and
 * return it. */
static unsigned long idmap_entry(IdMap *m, unsigned long key, unsigned long val)
{
    size_t lo = 0;
    size_t hi = m->len;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if (m->keys[mid] == key) {
            return m->vals[mid];
        }
        if (m->keys[mid] < key) {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    if (m->len == m->cap) {
        m->cap = m->cap ? m->cap * 2 : 16;
        m->keys = (unsigned long *)xrealloc(m->keys, m->cap * sizeof *m->keys);
        m->vals = (unsigned long *)xrealloc(m->vals, m->cap * sizeof *m->vals);
    }
    memmove(m->keys + lo + 1, m->keys + lo, (m->len - lo) * sizeof *m->keys);
    memmove(m->vals + lo + 1, m->vals + lo, (m->len - lo) * sizeof *m->vals);
    m->keys[lo] = key;
    m->vals[lo] = val;
    m->len++;
    return val;
}

static int missing(Disagreement *d, RecSpan span, const char *message)
{
    d->span = span;
    d->message = xstrdup(message);
    return 1;
}

int resolve_compare(const OccVec *ours, const OccVec *theirs, Disagreement *d)
{
    IdMap ours_to_theirs;
    IdMap theirs_to_ours;
    size_t i = 0;
    size_t j = 0;
    memset(&ours_to_theirs, 0, sizeof ours_to_theirs);
    memset(&theirs_to_ours, 0, sizeof theirs_to_ours);
    for (;;) {
        const Occurrence *our;
        const Occurrence *their;
        int c;
        if (i == ours->len && j == theirs->len) {
            return 0;
        }
        if (j == theirs->len) {
            return missing(d, ours->items[i].span,
                           "the reference has no occurrence here");
        }
        if (i == ours->len) {
            return missing(d, theirs->items[j].span, "we have no occurrence here");
        }
        our = &ours->items[i];
        their = &theirs->items[j];
        c = recspan_cmp(our->span, their->span);
        if (c < 0) {
            return missing(d, our->span, "the reference has no occurrence here");
        }
        if (c > 0) {
            return missing(d, their->span, "we have no occurrence here");
        }
        i++;
        j++;
        if (our->ns != their->ns) {
            d->span = our->span;
            d->message = str_printf("namespace: ours %s, reference %s",
                                    namespace_str(our->ns),
                                    namespace_str(their->ns));
            return 1;
        }
        if (our->target.kind == TARGET_TOP && their->target.kind == TARGET_TOP
            && target_eq(&our->target, &their->target)) {
            continue;
        }
        if (our->target.kind == TARGET_LOCAL && their->target.kind == TARGET_LOCAL) {
            unsigned long our_id = our->target.id;
            unsigned long their_id = their->target.id;
            unsigned long forward = idmap_entry(&ours_to_theirs, our_id, their_id);
            unsigned long backward = idmap_entry(&theirs_to_ours, their_id, our_id);
            if (forward != their_id || backward != our_id) {
                return missing(d, our->span,
                               "local binder: the spans of this binder differ "
                               "from the reference");
            }
            continue;
        }
        if ((our->target.kind == TARGET_SYNTAX && their->target.kind == TARGET_SYNTAX)
            || (our->target.kind == TARGET_ERROR
                && their->target.kind == TARGET_ERROR)) {
            continue;
        }
        {
            StrBuf b;
            sb_init(&b);
            sb_puts(&b, "target: ours ");
            target_write(&b, &our->target);
            sb_puts(&b, ", reference ");
            target_write(&b, &their->target);
            d->span = our->span;
            d->message = sb_take(&b);
            return 1;
        }
    }
}
