/* The evaluator: a lazy abstract machine over Core. See eval.h.
 *
 * Values live in a heap of words. Every object starts with a header word
 * that holds its kind and its size in payload words. The kinds:
 *
 *   INT        i                 a machine integer
 *   BIG        sign d0 .. dn     an Integer that does not fit in an INT
 *   CHAR       u                 a Unicode code point
 *   CON        con f1 .. fn      a constructor with its fields
 *   FUN        lam env           a closure of a CORE_LAM
 *   PAP        fun a1 .. ak      a partial application of a FUN or a PRIM
 *   PRIM       info              a primitive (allocated outside the heap)
 *   THUNK      code env          a suspended expression
 *   IND        value             a thunk that has been evaluated
 *   BLACKHOLE                    a thunk under evaluation
 *   ENV        parent s1 .. sn   a frame of local variables
 *   STR        bytes len         a string literal, not yet unpacked
 *   FWD        new               a forwarding pointer, during collection
 *
 * The machine keeps an explicit stack of continuation frames, so the
 * collector finds every live value in the stack, the registers and the
 * globals. The collector copies the live objects to a new space
 * (Cheney's algorithm) and doubles the space when it fills up.
 *
 * Exceptions unwind the stack. A `catch` pushes a K_CATCH frame with
 * the handler. A `raise` jumps (longjmp) to the innermost run of the
 * machine, which pops frames until it finds a K_CATCH frame. Every
 * thunk under evaluation on the way becomes a thunk again, so a later
 * use evaluates it again. A nested run that finds its K_STOP frame
 * jumps on to the run that called it. The outermost run reports an
 * uncaught exception and stops the program.
 *
 * Two rules keep allocation safe. First, a sequence of allocations
 * reserves its total size first, so no collection happens in the middle
 * of it. Second, a primitive reads its arguments before it allocates or
 * evaluates anything, because both can move objects, and it registers
 * every pointer it keeps across such a call as a root.
 */
#include <limits.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "eval.h"

/* --- Objects ------------------------------------------------------------ */

typedef union {
    Obj *p;
    long i;
    unsigned long u;
    const void *c;
} Word;

struct Obj {
    Word hdr;
};

enum {
    O_INT,
    O_BIG,
    O_CHAR,
    O_CON,
    O_FUN,
    O_PAP,
    O_PRIM,
    O_THUNK,
    O_IND,
    O_BLACKHOLE,
    O_ENV,
    O_STR,
    O_FWD
};

#define KIND(o) ((o)->hdr.u & 0xFF)
#define SIZE(o) ((o)->hdr.u >> 8)
#define W(o) ((Word *)(o) + 1)

static void set_hdr(Obj *o, unsigned long kind, unsigned long size)
{
    o->hdr.u = kind | (size << 8);
}

/* --- The heap ----------------------------------------------------------- */

static Word *heap;      /* the current space */
static Word *heap_end;
static Word *hp;        /* the next free word */
static unsigned long heap_size; /* in words */

static CoreProgram *prog;

/* Extra roots that primitives register (pointers to object pointers). */
static Obj ***roots;
static size_t nroots;
static size_t roots_cap;

static void root_push(Obj **p)
{
    if (nroots == roots_cap) {
        roots_cap = roots_cap ? roots_cap * 2 : 64;
        roots = (Obj ***)xrealloc(roots, roots_cap * sizeof *roots);
    }
    roots[nroots++] = p;
}

static void root_pop(void)
{
    nroots--;
}

/* --- The stack ---------------------------------------------------------- */

typedef enum {
    K_STOP,   /* the end of a run */
    K_UPDATE, /* a: the thunk to update with the value */
    K_ARG,    /* a: an argument waiting for a function */
    K_CASE,   /* code: the CORE_CASE; b: its environment */
    K_PRIM,   /* a: the arguments of a primitive (a PAP); n: the index of
                 the argument being evaluated */
    K_CATCH   /* a: the handler of an exception; b: the world */
} FrameKind;

typedef struct {
    FrameKind kind;
    Obj *a;
    Obj *b;
    const void *code;
    unsigned long n;
} Frame;

static Frame *stack;
static size_t sp;
static size_t stack_cap;

/* The machine registers. */
static Obj *reg_env;
static Obj *reg_val;
static Obj *reg_exc; /* the exception that is being raised */

/* The innermost run of the machine: a raise jumps to it. */
static jmp_buf *cur_jump;

static void push(FrameKind kind, Obj *a, Obj *b, const void *code, unsigned long n)
{
    Frame *f;
    if (sp == stack_cap) {
        stack_cap = stack_cap ? stack_cap * 2 : 4096;
        stack = (Frame *)xrealloc(stack, stack_cap * sizeof *stack);
    }
    f = &stack[sp++];
    f->kind = kind;
    f->a = a;
    f->b = b;
    f->code = code;
    f->n = n;
}

/* --- Run-time errors ---------------------------------------------------- */

static void runtime_error(const char *message)
{
    fflush(stdout);
    fprintf(stderr, "aihc-boot: %s\n", message);
    exit(1);
}

/* --- Garbage collection ------------------------------------------------- */

static int in_heap(const Obj *o, const Word *space, const Word *end)
{
    return (const Word *)o >= space && (const Word *)o < end;
}

static Word *to_space;
static Word *to_hp;
/* The other semispace, kept between collections so that a collection
 * does not allocate and free a space every time. */
static Word *spare;
static unsigned long spare_size;

static Obj *copy(Obj *o, const Word *from, const Word *from_end)
{
    Obj *n;
    unsigned long size;
    if (o == NULL || !in_heap(o, from, from_end)) {
        return o;
    }
    if (KIND(o) == O_FWD) {
        return W(o)[0].p;
    }
    size = SIZE(o);
    n = (Obj *)to_hp;
    to_hp += 1 + size;
    memcpy(n, o, (1 + size) * sizeof(Word));
    set_hdr(o, O_FWD, size);
    W(o)[0].p = n;
    return n;
}

/* The range of pointer fields of an object: from *first to *last
 * exclusive. */
static void pointer_fields(const Obj *o, unsigned long *first, unsigned long *last)
{
    switch (KIND(o)) {
    case O_CON:
        *first = 1;
        *last = SIZE(o);
        break;
    case O_FUN:
    case O_THUNK:
    case O_BLACKHOLE: /* keeps its environment: an exception restores it */
        *first = 1;
        *last = 2;
        break;
    case O_PAP:
    case O_ENV:
        *first = 0;
        *last = SIZE(o);
        break;
    case O_IND:
        *first = 0;
        *last = 1;
        break;
    default:
        *first = 0;
        *last = 0;
        break;
    }
}

static void gc(unsigned long need)
{
    Word *from = heap;
    Word *from_end = heap_end;
    unsigned long new_size = heap_size;
    size_t i;
    Word *scan;
    unsigned long live;
    for (;;) {
        if (spare != NULL && spare_size == new_size) {
            to_space = spare;
            spare = NULL;
        } else {
            free(spare);
            spare = NULL;
            to_space = (Word *)xmalloc(new_size * sizeof(Word));
        }
        to_hp = to_space;
        for (i = 0; i < prog->globals.len; i++) {
            Global *g = (Global *)prog->globals.items[i];
            g->value = copy((Obj *)g->value, from, from_end);
        }
        for (i = 0; i < sp; i++) {
            stack[i].a = copy(stack[i].a, from, from_end);
            stack[i].b = copy(stack[i].b, from, from_end);
        }
        for (i = 0; i < nroots; i++) {
            *roots[i] = copy(*roots[i], from, from_end);
        }
        reg_env = copy(reg_env, from, from_end);
        reg_val = copy(reg_val, from, from_end);
        reg_exc = copy(reg_exc, from, from_end);
        scan = to_space;
        while (scan < to_hp) {
            Obj *o = (Obj *)scan;
            unsigned long first;
            unsigned long last;
            unsigned long k;
            pointer_fields(o, &first, &last);
            for (k = first; k < last; k++) {
                W(o)[k].p = copy(W(o)[k].p, from, from_end);
            }
            scan += 1 + SIZE(o);
        }
        live = (unsigned long)(to_hp - to_space);
        if ((live + need) * 2 <= new_size) {
            break;
        }
        /* The space is more than half full after the collection: collect
         * again into a space twice as large. The forwarding pointers in
         * the old space are still valid, so the copied objects become
         * the new from-space. */
        free(from == heap ? NULL : from);
        from = to_space;
        from_end = to_hp;
        new_size *= 2;
    }
    if (from != heap) {
        free(from);
    }
    if (new_size == heap_size) {
        spare = heap;
        spare_size = heap_size;
    } else {
        free(heap);
    }
    heap = to_space;
    heap_end = to_space + new_size;
    hp = to_hp;
    heap_size = new_size;
}

/* Make room for words (headers included) without a collection in the
 * allocations that follow. */
static void reserve(unsigned long words)
{
    if (hp + words > heap_end) {
        gc(words);
    }
}

/* Allocate an object of size payload words. The caller has reserved the
 * room. */
static Obj *alloc(unsigned long kind, unsigned long size)
{
    Obj *o = (Obj *)hp;
    hp += 1 + size;
    set_hdr(o, kind, size);
    return o;
}

/* Allocate an object outside the heap, for values that live for the
 * whole run and point to nothing in the heap. */
static Obj *alloc_static(unsigned long kind, unsigned long size)
{
    Obj *o = (Obj *)xcalloc(1 + size, sizeof(Word));
    set_hdr(o, kind, size);
    return o;
}

/* --- Small values ------------------------------------------------------- */

static Obj *small_ints[256 + 16];
static Obj *small_chars[256];

static Obj *make_int(long i)
{
    Obj *o;
    if (i >= -16 && i < 256) {
        return small_ints[i + 16];
    }
    reserve(2);
    o = alloc(O_INT, 1);
    W(o)[0].i = i;
    return o;
}

static Obj *make_char(unsigned long c)
{
    Obj *o;
    if (c < 256) {
        return small_chars[c];
    }
    reserve(2);
    o = alloc(O_CHAR, 1);
    W(o)[0].u = c;
    return o;
}

/* The static object of a nullary constructor. */
static Obj *nullary(ConInfo *con)
{
    if (con->value == NULL) {
        Obj *o = alloc_static(O_CON, 1);
        W(o)[0].c = con;
        con->value = o;
    }
    return (Obj *)con->value;
}

static Obj *make_bool(int b)
{
    return nullary(b ? prog->con_true : prog->con_false);
}

/* --- Atoms: the arguments of applications and constructors -------------- */

/* The words that atom needs. */
static unsigned long atom_size(const Core *e)
{
    unsigned long n;
    size_t i;
    switch (e->kind) {
    case CORE_VAR:
    case CORE_GLOBAL:
        return 0;
    case CORE_INT:
    case CORE_CHAR:
        return 2;
    case CORE_STR:
        return 3;
    case CORE_LAM:
        return 3;
    case CORE_CON:
        if (e->args.len == 0) {
            return 0;
        }
        n = 2 + e->args.len;
        for (i = 0; i < e->args.len; i++) {
            n += atom_size((const Core *)e->args.items[i]);
        }
        return n;
    default:
        return 3;
    }
}

static Obj *lookup_var(const Core *e, Obj *env)
{
    unsigned long up = e->up;
    while (up > 0) {
        env = W(env)[0].p;
        up--;
    }
    return W(env)[1 + e->slot].p;
}

/* The value of an atom, allocated from reserved room. A variable is its
 * value; a literal, a lambda and a constructor are built directly; any
 * other expression becomes a thunk. */
static Obj *atom(const Core *e, Obj *env)
{
    Obj *o;
    size_t i;
    switch (e->kind) {
    case CORE_VAR:
        return lookup_var(e, env);
    case CORE_GLOBAL:
        return (Obj *)e->global->value;
    case CORE_INT:
        if (e->ival >= -16 && e->ival < 256) {
            return small_ints[e->ival + 16];
        }
        o = alloc(O_INT, 1);
        W(o)[0].i = e->ival;
        return o;
    case CORE_CHAR:
        if (e->cval < 256) {
            return small_chars[e->cval];
        }
        o = alloc(O_CHAR, 1);
        W(o)[0].u = e->cval;
        return o;
    case CORE_STR:
        o = alloc(O_STR, 2);
        W(o)[0].c = e->sval;
        W(o)[1].u = e->slen;
        return o;
    case CORE_LAM:
        o = alloc(O_FUN, 2);
        W(o)[0].c = e;
        W(o)[1].p = env;
        return o;
    case CORE_CON:
        if (e->args.len == 0) {
            return nullary(e->con);
        }
        o = alloc(O_CON, 1 + e->args.len);
        W(o)[0].c = e->con;
        for (i = 0; i < e->args.len; i++) {
            W(o)[1 + i].p = atom((const Core *)e->args.items[i], env);
        }
        return o;
    default:
        o = alloc(O_THUNK, 2);
        W(o)[0].c = e;
        W(o)[1].p = env;
        return o;
    }
}

/* Whether an atom reads a slot of the innermost frame: a variable of
 * the let group itself, or a constructor with such a variable as a
 * field. */
static int reads_own_frame(const Core *e)
{
    size_t i;
    switch (e->kind) {
    case CORE_VAR:
        return e->up == 0;
    case CORE_CON:
        for (i = 0; i < e->args.len; i++) {
            if (reads_own_frame((const Core *)e->args.items[i])) {
                return 1;
            }
        }
        return 0;
    default:
        return 0;
    }
}

/* A let binding. A binder that reads a slot of its own group may read
 * a slot that is not filled yet, so it becomes a thunk. When the thunk
 * runs, the slots are filled, and a recursive structure such as
 * `xs = x : xs` closes through the thunk's indirection. */
static unsigned long bind_size(const Core *e)
{
    return reads_own_frame(e) ? 3 : atom_size(e);
}

static Obj *bind_atom(const Core *e, Obj *frame)
{
    Obj *o;
    if (!reads_own_frame(e)) {
        return atom(e, frame);
    }
    o = alloc(O_THUNK, 2);
    W(o)[0].c = e;
    W(o)[1].p = frame;
    return o;
}

/* --- Primitives --------------------------------------------------------- */

static const PrimInfo *prim_of(const Obj *o)
{
    return (const PrimInfo *)W(o)[0].c;
}

static long int_of(const Obj *o)
{
    return W(o)[0].i;
}

static unsigned long char_of(const Obj *o)
{
    return W(o)[0].u;
}

/* A constructor application with one field: `IORes x`. */
static Obj *io_result(Obj *x)
{
    Obj *o;
    root_push(&x);
    reserve(3);
    root_pop();
    o = alloc(O_CON, 2);
    W(o)[0].c = prog->con_iores;
    W(o)[1].p = x;
    return o;
}

static Obj *run(Obj *start);

/* Evaluate an object to weak head normal form, from inside a primitive.
 * Objects may move: the caller keeps no other pointer across the call
 * unless it is registered as a root. */
static Obj *force(Obj *o)
{
    return run(o);
}

/* Read a Haskell String (a list of Char) into a UTF-8 buffer, forcing
 * it. */
static char *force_string(Obj *list)
{
    StrBuf b;
    sb_init(&b);
    root_push(&list);
    for (;;) {
        Obj *cell = force(list);
        const ConInfo *con;
        Obj *head;
        if (KIND(cell) != O_CON) {
            break;
        }
        con = (const ConInfo *)W(cell)[0].c;
        if (con->arity != 2) {
            break;
        }
        head = W(cell)[1].p;
        list = W(cell)[2].p;
        head = force(head);
        if (KIND(head) != O_CHAR) {
            break;
        }
        sb_put_utf8(&b, char_of(head));
    }
    root_pop();
    return sb_take(&b);
}

/* A Haskell String from a C string: a list of Char, built from the end.
 * The characters are ASCII, so they are static. */
static Obj *make_string(const char *s)
{
    size_t n = strlen(s);
    Obj *list;
    size_t i;
    reserve(3 * (unsigned long)n);
    list = nullary(prog->con_nil);
    i = n;
    while (i > 0) {
        Obj *cell = alloc(O_CON, 3);
        i--;
        W(cell)[0].c = prog->con_cons;
        W(cell)[1].p = small_chars[(unsigned char)s[i]];
        W(cell)[2].p = list;
        list = cell;
    }
    return list;
}

static Obj *p_int_add(Obj **a) { return make_int(int_of(a[0]) + int_of(a[1])); }
static Obj *p_int_sub(Obj **a) { return make_int(int_of(a[0]) - int_of(a[1])); }
static Obj *p_int_mul(Obj **a) { return make_int(int_of(a[0]) * int_of(a[1])); }
static Obj *p_int_negate(Obj **a) { return make_int(-int_of(a[0])); }

static Obj *p_int_quot(Obj **a)
{
    long y = int_of(a[1]);
    if (y == 0) {
        runtime_error("divide by zero");
    }
    return make_int(int_of(a[0]) / y);
}

static Obj *p_int_rem(Obj **a)
{
    long y = int_of(a[1]);
    if (y == 0) {
        runtime_error("divide by zero");
    }
    return make_int(int_of(a[0]) % y);
}

/* Division that rounds toward negative infinity, as Haskell's div. */
static Obj *p_int_div(Obj **a)
{
    long x = int_of(a[0]);
    long y = int_of(a[1]);
    long q;
    if (y == 0) {
        runtime_error("divide by zero");
    }
    q = x / y;
    if ((x % y != 0) && ((x < 0) != (y < 0))) {
        q--;
    }
    return make_int(q);
}

static Obj *p_int_mod(Obj **a)
{
    long x = int_of(a[0]);
    long y = int_of(a[1]);
    long r;
    if (y == 0) {
        runtime_error("divide by zero");
    }
    r = x % y;
    if (r != 0 && ((r < 0) != (y < 0))) {
        r += y;
    }
    return make_int(r);
}

static Obj *p_int_eq(Obj **a) { return make_bool(int_of(a[0]) == int_of(a[1])); }
static Obj *p_int_ne(Obj **a) { return make_bool(int_of(a[0]) != int_of(a[1])); }
static Obj *p_int_lt(Obj **a) { return make_bool(int_of(a[0]) < int_of(a[1])); }
static Obj *p_int_le(Obj **a) { return make_bool(int_of(a[0]) <= int_of(a[1])); }
static Obj *p_int_gt(Obj **a) { return make_bool(int_of(a[0]) > int_of(a[1])); }
static Obj *p_int_ge(Obj **a) { return make_bool(int_of(a[0]) >= int_of(a[1])); }

static Obj *p_char_eq(Obj **a) { return make_bool(char_of(a[0]) == char_of(a[1])); }
static Obj *p_char_ne(Obj **a) { return make_bool(char_of(a[0]) != char_of(a[1])); }
static Obj *p_char_lt(Obj **a) { return make_bool(char_of(a[0]) < char_of(a[1])); }
static Obj *p_char_le(Obj **a) { return make_bool(char_of(a[0]) <= char_of(a[1])); }
static Obj *p_char_gt(Obj **a) { return make_bool(char_of(a[0]) > char_of(a[1])); }
static Obj *p_char_ge(Obj **a) { return make_bool(char_of(a[0]) >= char_of(a[1])); }

static Obj *p_ord(Obj **a) { return make_int((long)char_of(a[0])); }

static Obj *p_chr(Obj **a)
{
    long i = int_of(a[0]);
    if (i < 0 || i > 0x10FFFF) {
        runtime_error("chr: bad argument");
    }
    return make_char((unsigned long)i);
}

static Obj *p_int_show(Obj **a)
{
    char buf[32];
    sprintf(buf, "%ld", int_of(a[0]));
    return make_string(buf);
}

/* --- Integer ------------------------------------------------------------ */

/* An Integer is an INT when it fits in a long, and a BIG when it does
 * not. A BIG holds the sign (1 or -1) and the magnitude in limbs of 16
 * bits, the least significant limb first, one limb in each word. C89
 * makes an unsigned long at least 32 bits wide, so the product of two
 * limbs fits in it. The arithmetic works on a Big: a copy of the value
 * outside the heap. Speed matters little here, because almost every
 * Integer in the vendored tree is small and takes the INT path. */

#define LIMB_BITS 16
#define LIMB_MASK 0xFFFFUL
#define LONG_BITS (sizeof(long) * CHAR_BIT)
/* The number of limbs that always fit in a long. */
#define LONG_LIMBS ((unsigned long)(LONG_BITS / LIMB_BITS))

typedef struct {
    int neg;
    unsigned long n; /* the number of limbs; 0 for zero */
    unsigned long *d;
} Big;

static void big_alloc(Big *b, unsigned long n)
{
    b->neg = 0;
    b->n = n;
    b->d = (unsigned long *)xcalloc(n + 1, sizeof(unsigned long));
}

/* Remove the leading zero limbs. Zero has no sign. */
static void big_trim(Big *b)
{
    while (b->n > 0 && b->d[b->n - 1] == 0) {
        b->n--;
    }
    if (b->n == 0) {
        b->neg = 0;
    }
}

static void big_from_long(Big *b, long x)
{
    unsigned long u = x < 0 ? 0UL - (unsigned long)x : (unsigned long)x;
    unsigned long i;
    big_alloc(b, LONG_LIMBS + 1);
    for (i = 0; i < b->n; i++) {
        b->d[i] = u & LIMB_MASK;
        u >>= LIMB_BITS;
    }
    b->neg = x < 0;
    big_trim(b);
}

static void big_copy(const Big *a, Big *b)
{
    big_alloc(b, a->n);
    memcpy(b->d, a->d, a->n * sizeof(unsigned long));
    b->neg = a->neg;
}

/* The Big of an Integer object in weak head normal form. */
static void big_of(const Obj *o, Big *b)
{
    unsigned long i;
    if (KIND(o) == O_INT) {
        big_from_long(b, int_of(o));
        return;
    }
    big_alloc(b, SIZE(o) - 1);
    b->neg = W(o)[0].i < 0;
    for (i = 0; i < b->n; i++) {
        b->d[i] = W(o)[1 + i].u;
    }
}

/* The Integer object of a Big. It frees the limbs. */
static Obj *big_obj(Big *b)
{
    Obj *o;
    unsigned long i;
    big_trim(b);
    if (b->n <= LONG_LIMBS) {
        unsigned long u = 0;
        i = b->n;
        while (i > 0) {
            i--;
            u = (u << LIMB_BITS) | b->d[i];
        }
        if (!b->neg && u <= (unsigned long)LONG_MAX) {
            free(b->d);
            return make_int((long)u);
        }
        if (b->neg && u <= (unsigned long)LONG_MAX + 1) {
            free(b->d);
            return make_int(u == (unsigned long)LONG_MAX + 1 ? LONG_MIN : -(long)u);
        }
    }
    reserve(2 + b->n);
    o = alloc(O_BIG, 1 + b->n);
    W(o)[0].i = b->neg ? -1 : 1;
    for (i = 0; i < b->n; i++) {
        W(o)[1 + i].u = b->d[i];
    }
    free(b->d);
    return o;
}

/* Compare the magnitudes. Leading zero limbs are allowed. */
static int mag_cmp(const Big *a, const Big *b)
{
    unsigned long an = a->n;
    unsigned long bn = b->n;
    while (an > 0 && a->d[an - 1] == 0) {
        an--;
    }
    while (bn > 0 && b->d[bn - 1] == 0) {
        bn--;
    }
    if (an != bn) {
        return an < bn ? -1 : 1;
    }
    while (an > 0) {
        an--;
        if (a->d[an] != b->d[an]) {
            return a->d[an] < b->d[an] ? -1 : 1;
        }
    }
    return 0;
}

static int big_cmp(const Big *a, const Big *b)
{
    int c;
    if (a->neg != b->neg) {
        return a->neg ? -1 : 1;
    }
    c = mag_cmp(a, b);
    return a->neg ? -c : c;
}

/* r := |r| - |b|, with |r| >= |b|. */
static void mag_sub_from(Big *r, const Big *b)
{
    unsigned long borrow = 0;
    unsigned long i;
    for (i = 0; i < r->n; i++) {
        unsigned long x = r->d[i];
        unsigned long y = (i < b->n ? b->d[i] : 0) + borrow;
        if (x >= y) {
            r->d[i] = x - y;
            borrow = 0;
        } else {
            r->d[i] = x + (LIMB_MASK + 1) - y;
            borrow = 1;
        }
    }
}

/* r := a + b, where b has the sign bneg. */
static void big_add(const Big *a, const Big *b, int bneg, Big *r)
{
    if (a->neg == bneg) {
        unsigned long n = a->n > b->n ? a->n : b->n;
        unsigned long carry = 0;
        unsigned long i;
        big_alloc(r, n + 1);
        for (i = 0; i < n; i++) {
            unsigned long s = carry + (i < a->n ? a->d[i] : 0) + (i < b->n ? b->d[i] : 0);
            r->d[i] = s & LIMB_MASK;
            carry = s >> LIMB_BITS;
        }
        r->d[n] = carry;
        r->neg = a->neg;
    } else if (mag_cmp(a, b) >= 0) {
        big_copy(a, r);
        mag_sub_from(r, b);
    } else {
        big_copy(b, r);
        mag_sub_from(r, a);
        r->neg = bneg;
    }
    big_trim(r);
}

static void big_mul(const Big *a, const Big *b, Big *r)
{
    unsigned long i;
    unsigned long j;
    big_alloc(r, a->n + b->n);
    for (i = 0; i < a->n; i++) {
        unsigned long carry = 0;
        for (j = 0; j < b->n; j++) {
            unsigned long t = a->d[i] * b->d[j] + r->d[i + j] + carry;
            r->d[i + j] = t & LIMB_MASK;
            carry = t >> LIMB_BITS;
        }
        r->d[i + b->n] = carry;
    }
    r->neg = a->neg != b->neg;
    big_trim(r);
}

/* q := |a| / |b| and r := |a| mod |b|, without signs. b is not zero. */
static void mag_divmod(const Big *a, const Big *b, Big *q, Big *r)
{
    unsigned long i;
    big_alloc(q, a->n);
    if (b->n == 1) {
        /* One limb: the remainder stays below 2^16, so a limb shifted
         * into it stays below 2^32. */
        unsigned long rem = 0;
        unsigned long y = b->d[0];
        i = a->n;
        while (i > 0) {
            i--;
            rem = (rem << LIMB_BITS) | a->d[i];
            q->d[i] = rem / y;
            rem %= y;
        }
        big_alloc(r, 1);
        r->d[0] = rem;
    } else {
        /* Long division, one bit at a time. */
        big_alloc(r, b->n + 1);
        i = a->n * LIMB_BITS;
        while (i > 0) {
            unsigned long carry;
            unsigned long k;
            i--;
            carry = (a->d[i / LIMB_BITS] >> (i % LIMB_BITS)) & 1;
            for (k = 0; k < r->n; k++) {
                unsigned long t = (r->d[k] << 1) | carry;
                r->d[k] = t & LIMB_MASK;
                carry = t >> LIMB_BITS;
            }
            if (mag_cmp(r, b) >= 0) {
                mag_sub_from(r, b);
                q->d[i / LIMB_BITS] |= 1UL << (i % LIMB_BITS);
            }
        }
    }
    big_trim(q);
    big_trim(r);
}

/* Divide a by b. The quotient rounds toward zero (quot and rem) or,
 * when floored is set, toward negative infinity (div and mod). */
static void big_divide(const Big *a, const Big *b, int floored, Big *q, Big *r)
{
    if (b->n == 0) {
        runtime_error("divide by zero");
    }
    mag_divmod(a, b, q, r);
    q->neg = q->n > 0 && a->neg != b->neg;
    r->neg = r->n > 0 && a->neg;
    if (floored && r->n > 0 && a->neg != b->neg) {
        Big one;
        Big t;
        big_from_long(&one, 1);
        big_add(q, &one, 1, &t);
        free(q->d);
        *q = t;
        big_add(r, b, b->neg, &t);
        free(r->d);
        *r = t;
        free(one.d);
    }
}

/* The greatest common divisor, by Euclid's algorithm. */
static void big_gcd(const Big *a, const Big *b, Big *r)
{
    Big x;
    Big y;
    big_copy(a, &x);
    big_copy(b, &y);
    while (y.n > 0) {
        Big q;
        Big m;
        mag_divmod(&x, &y, &q, &m);
        free(q.d);
        free(x.d);
        x = y;
        y = m;
    }
    free(y.d);
    x.neg = 0;
    *r = x;
}

enum { I_ADD, I_SUB, I_MUL, I_QUOT, I_REM, I_DIV, I_MOD, I_GCD };

/* A binary operation on two Integers in weak head normal form. */
static Obj *integer_op(Obj **a, int op)
{
    Big x;
    Big y;
    Big q;
    Big r;
    if (KIND(a[0]) == O_INT && KIND(a[1]) == O_INT) {
        /* The fast path, when the result fits in a long. */
        long i = int_of(a[0]);
        long j = int_of(a[1]);
        long half = 1L << (LONG_BITS / 2 - 1);
        int no_overflow = !(i == LONG_MIN && j == -1);
        switch (op) {
        case I_ADD:
            if (j >= 0 ? i <= LONG_MAX - j : i >= LONG_MIN - j) {
                return make_int(i + j);
            }
            break;
        case I_SUB:
            if (j >= 0 ? i >= LONG_MIN + j : i <= LONG_MAX + j) {
                return make_int(i - j);
            }
            break;
        case I_MUL:
            if (i >= -half && i <= half && j >= -half && j <= half) {
                return make_int(i * j);
            }
            break;
        case I_QUOT:
            if (no_overflow) {
                return p_int_quot(a);
            }
            break;
        case I_REM:
            if (no_overflow) {
                return p_int_rem(a);
            }
            break;
        case I_DIV:
            if (no_overflow) {
                return p_int_div(a);
            }
            break;
        case I_MOD:
            if (no_overflow) {
                return p_int_mod(a);
            }
            break;
        default:
            break;
        }
    }
    big_of(a[0], &x);
    big_of(a[1], &y);
    switch (op) {
    case I_ADD:
        big_add(&x, &y, y.neg, &r);
        break;
    case I_SUB:
        big_add(&x, &y, y.n > 0 && !y.neg, &r);
        break;
    case I_MUL:
        big_mul(&x, &y, &r);
        break;
    case I_GCD:
        big_gcd(&x, &y, &r);
        break;
    default:
        big_divide(&x, &y, op == I_DIV || op == I_MOD, &q, &r);
        if (op == I_QUOT || op == I_DIV) {
            free(r.d);
            r = q;
        } else {
            free(q.d);
        }
        break;
    }
    free(x.d);
    free(y.d);
    return big_obj(&r);
}

static int integer_cmp(Obj **a)
{
    Big x;
    Big y;
    int c;
    if (KIND(a[0]) == O_INT && KIND(a[1]) == O_INT) {
        long i = int_of(a[0]);
        long j = int_of(a[1]);
        return i < j ? -1 : i > j;
    }
    big_of(a[0], &x);
    big_of(a[1], &y);
    c = big_cmp(&x, &y);
    free(x.d);
    free(y.d);
    return c;
}

static Obj *p_integer_add(Obj **a) { return integer_op(a, I_ADD); }
static Obj *p_integer_sub(Obj **a) { return integer_op(a, I_SUB); }
static Obj *p_integer_mul(Obj **a) { return integer_op(a, I_MUL); }
static Obj *p_integer_quot(Obj **a) { return integer_op(a, I_QUOT); }
static Obj *p_integer_rem(Obj **a) { return integer_op(a, I_REM); }
static Obj *p_integer_div(Obj **a) { return integer_op(a, I_DIV); }
static Obj *p_integer_mod(Obj **a) { return integer_op(a, I_MOD); }
static Obj *p_integer_gcd(Obj **a) { return integer_op(a, I_GCD); }
static Obj *p_integer_eq(Obj **a) { return make_bool(integer_cmp(a) == 0); }
static Obj *p_integer_ne(Obj **a) { return make_bool(integer_cmp(a) != 0); }
static Obj *p_integer_lt(Obj **a) { return make_bool(integer_cmp(a) < 0); }
static Obj *p_integer_le(Obj **a) { return make_bool(integer_cmp(a) <= 0); }
static Obj *p_integer_gt(Obj **a) { return make_bool(integer_cmp(a) > 0); }
static Obj *p_integer_ge(Obj **a) { return make_bool(integer_cmp(a) >= 0); }

static Obj *p_integer_negate(Obj **a)
{
    Big x;
    if (KIND(a[0]) == O_INT && int_of(a[0]) != LONG_MIN) {
        return make_int(-int_of(a[0]));
    }
    big_of(a[0], &x);
    x.neg = x.n > 0 && !x.neg;
    return big_obj(&x);
}

/* An Int is an Integer that fits in a long. */
static Obj *p_integer_from_int(Obj **a)
{
    return a[0];
}

/* The low bits of an Integer, in two's complement, as GHC does. */
static Obj *p_integer_to_int(Obj **a)
{
    unsigned long u = 0;
    unsigned long i;
    if (KIND(a[0]) == O_INT) {
        return a[0];
    }
    i = SIZE(a[0]) - 1;
    if (i > LONG_LIMBS) {
        i = LONG_LIMBS;
    }
    while (i > 0) {
        i--;
        u = (u << LIMB_BITS) | W(a[0])[1 + i].u;
    }
    if (W(a[0])[0].i < 0) {
        u = 0UL - u;
    }
    return make_int(u <= (unsigned long)LONG_MAX ? (long)u : -(long)(ULONG_MAX - u) - 1);
}

/* Raise an exception: jump to the innermost run of the machine, which
 * unwinds the stack. */
static void raise_exception(Obj *exc)
{
    reg_exc = exc;
    longjmp(*cur_jump, 1);
}

/* `raise e`: the argument is not evaluated, as in GHC. */
static Obj *p_raise(Obj **a)
{
    raise_exception(a[0]);
    return NULL;
}

/* `catch action handler world`: run the action on the world under a
 * K_CATCH frame. The machine passes the result of the action through
 * the frame. An exception applies the handler to the exception and the
 * world. */
static Obj *p_catch(Obj **a)
{
    Obj *action = a[0];
    push(K_CATCH, a[1], a[2], NULL, 0);
    push(K_ARG, a[2], NULL, NULL, 0);
    return action;
}

static Obj *p_put_char(Obj **a)
{
    StrBuf b;
    sb_init(&b);
    sb_put_utf8(&b, char_of(a[0]));
    fwrite(b.s, 1, b.len, stdout);
    free(b.s);
    return io_result(nullary(prog->con_unit));
}

/* One code point from stdin, or -1 at the end of the input. Invalid
 * bytes come back as U+FFFD. */
static Obj *p_get_char(Obj **a)
{
    int c = getc(stdin);
    unsigned long cp;
    int extra;
    int i;
    (void)a;
    if (c == EOF) {
        return io_result(make_int(-1));
    }
    if (c < 0x80) {
        return io_result(make_int(c));
    }
    if ((c & 0xE0) == 0xC0) {
        cp = (unsigned long)c & 0x1F;
        extra = 1;
    } else if ((c & 0xF0) == 0xE0) {
        cp = (unsigned long)c & 0x0F;
        extra = 2;
    } else if ((c & 0xF8) == 0xF0) {
        cp = (unsigned long)c & 0x07;
        extra = 3;
    } else {
        return io_result(make_int(0xFFFD));
    }
    for (i = 0; i < extra; i++) {
        int d = getc(stdin);
        if (d == EOF || (d & 0xC0) != 0x80) {
            if (d != EOF) {
                ungetc(d, stdin);
            }
            return io_result(make_int(0xFFFD));
        }
        cp = (cp << 6) | ((unsigned long)d & 0x3F);
    }
    return io_result(make_int((long)cp));
}

static Obj *p_flush(Obj **a)
{
    (void)a;
    fflush(stdout);
    return io_result(nullary(prog->con_unit));
}

/* The argument is evaluated and returned: `seq` on one argument. */
static Obj *p_whnf(Obj **a)
{
    return a[0];
}

/* The argument, unevaluated, at another type. */
static Obj *p_unsafe_coerce(Obj **a)
{
    return a[0];
}

static const PrimInfo prims[] = {
    {"intAdd", 2, 3, p_int_add},
    {"intSub", 2, 3, p_int_sub},
    {"intMul", 2, 3, p_int_mul},
    {"intQuot", 2, 3, p_int_quot},
    {"intRem", 2, 3, p_int_rem},
    {"intDiv", 2, 3, p_int_div},
    {"intMod", 2, 3, p_int_mod},
    {"intNegate", 1, 1, p_int_negate},
    {"intEq", 2, 3, p_int_eq},
    {"intNe", 2, 3, p_int_ne},
    {"intLt", 2, 3, p_int_lt},
    {"intLe", 2, 3, p_int_le},
    {"intGt", 2, 3, p_int_gt},
    {"intGe", 2, 3, p_int_ge},
    {"charEq", 2, 3, p_char_eq},
    {"charNe", 2, 3, p_char_ne},
    {"charLt", 2, 3, p_char_lt},
    {"charLe", 2, 3, p_char_le},
    {"charGt", 2, 3, p_char_gt},
    {"charGe", 2, 3, p_char_ge},
    {"ord", 1, 1, p_ord},
    {"chr", 1, 1, p_chr},
    {"intShow", 1, 1, p_int_show},
    {"raise", 1, 0, p_raise},
    {"catch", 3, 0, p_catch},
    {"integerFromInt", 1, 1, p_integer_from_int},
    {"integerToInt", 1, 1, p_integer_to_int},
    {"integerAdd", 2, 3, p_integer_add},
    {"integerSub", 2, 3, p_integer_sub},
    {"integerMul", 2, 3, p_integer_mul},
    {"integerNegate", 1, 1, p_integer_negate},
    {"integerQuot", 2, 3, p_integer_quot},
    {"integerRem", 2, 3, p_integer_rem},
    {"integerDiv", 2, 3, p_integer_div},
    {"integerMod", 2, 3, p_integer_mod},
    {"integerGcd", 2, 3, p_integer_gcd},
    {"integerEq", 2, 3, p_integer_eq},
    {"integerNe", 2, 3, p_integer_ne},
    {"integerLt", 2, 3, p_integer_lt},
    {"integerLe", 2, 3, p_integer_le},
    {"integerGt", 2, 3, p_integer_gt},
    {"integerGe", 2, 3, p_integer_ge},
    {"putChar", 2, 1, p_put_char},
    {"getChar", 1, 0, p_get_char},
    {"flush", 1, 0, p_flush},
    {"whnf", 1, 1, p_whnf},
    {"unsafeCoerce", 1, 0, p_unsafe_coerce},
    {NULL, 0, 0, NULL}
};

const PrimInfo *prim_lookup(const char *name)
{
    size_t i;
    for (i = 0; prims[i].name != NULL; i++) {
        if (str_eq(prims[i].name, name)) {
            return &prims[i];
        }
    }
    return NULL;
}

/* --- The machine -------------------------------------------------------- */

#define MAX_PRIM_ARGS 8

/* Call a primitive on its collected arguments: the PAP object of the
 * K_PRIM frame on top of the stack. The frame keeps the arguments alive
 * during the call. A primitive can push frames of its own (`catch`
 * does), so the K_PRIM frame is then removed from under them. */
static Obj *call_prim(const PrimInfo *info)
{
    Obj *argv[MAX_PRIM_ARGS];
    unsigned long j;
    size_t at = sp - 1;
    Obj *args = stack[at].a;
    Obj *result;
    for (j = 0; j < info->arity && j < MAX_PRIM_ARGS; j++) {
        argv[j] = W(args)[1 + j].p;
    }
    result = info->fn(argv);
    memmove(&stack[at], &stack[at + 1], (sp - at - 1) * sizeof *stack);
    sp--;
    return result;
}

/* The number of K_ARG frames on top of the stack. */
static unsigned long args_on_stack(void)
{
    unsigned long n = 0;
    while (n < sp && stack[sp - 1 - n].kind == K_ARG) {
        n++;
    }
    return n;
}

/* The alternative of a case for a value. */
static const CoreAlt *select_alt(const Core *c, const Obj *v)
{
    size_t i;
    const CoreAlt *dflt = NULL;
    for (i = 0; i < c->alts.len; i++) {
        const CoreAlt *a = (const CoreAlt *)c->alts.items[i];
        switch (a->kind) {
        case ALT_CON:
            if (KIND(v) == O_CON && (const ConInfo *)W(v)[0].c == a->con) {
                return a;
            }
            break;
        case ALT_INT:
            if (KIND(v) == O_INT && int_of(v) == a->ival) {
                return a;
            }
            break;
        case ALT_CHAR:
            if (KIND(v) == O_CHAR && char_of(v) == a->cval) {
                return a;
            }
            break;
        case ALT_DEFAULT:
            if (dflt == NULL) {
                dflt = a;
            }
            break;
        }
    }
    return dflt;
}

/* Run the machine from an object until the K_STOP frame on the stack
 * is reached. Returns the value in weak head normal form. */
static Obj *machine(Obj *start)
{
    const Core *code = NULL;
    Obj *o;
    o = start;
    goto enter;

eval:
    switch (code->kind) {
    case CORE_VAR:
        o = lookup_var(code, reg_env);
        goto enter;
    case CORE_GLOBAL:
        o = (Obj *)code->global->value;
        goto enter;
    case CORE_INT:
        reg_val = make_int(code->ival);
        goto ret;
    case CORE_CHAR:
        reg_val = make_char(code->cval);
        goto ret;
    case CORE_STR:
    case CORE_LAM:
    case CORE_CON:
        reserve(atom_size(code));
        o = atom(code, reg_env);
        goto enter;
    case CORE_APP: {
        /* Push the arguments, the first on top, then evaluate the
         * function. */
        size_t i = code->args.len;
        while (i > 0) {
            const Core *arg;
            i--;
            arg = (const Core *)code->args.items[i];
            reserve(atom_size(arg));
            push(K_ARG, atom(arg, reg_env), NULL, NULL, 0);
        }
        code = code->fun;
        goto eval;
    }
    case CORE_LET: {
        /* One frame for the binders. Its slots are atoms built in the
         * frame itself, so the bindings can be recursive. */
        unsigned long n = code->binds.len;
        unsigned long words = 2 + n;
        unsigned long i;
        Obj *frame;
        for (i = 0; i < n; i++) {
            words += bind_size((const Core *)code->binds.items[i]);
        }
        reserve(words);
        frame = alloc(O_ENV, 1 + n);
        W(frame)[0].p = reg_env;
        for (i = 0; i < n; i++) {
            W(frame)[1 + i].p = NULL;
        }
        for (i = 0; i < n; i++) {
            W(frame)[1 + i].p = bind_atom((const Core *)code->binds.items[i], frame);
        }
        reg_env = frame;
        code = code->body;
        goto eval;
    }
    case CORE_CASE:
        push(K_CASE, NULL, reg_env, code, 0);
        code = code->scrut;
        goto eval;
    }

enter:
    /* Evaluate an object: follow indirections and enter thunks. */
    for (;;) {
        switch (KIND(o)) {
        case O_IND:
            o = W(o)[0].p;
            continue;
        case O_THUNK:
            push(K_UPDATE, o, NULL, NULL, 0);
            code = (const Core *)W(o)[0].c;
            reg_env = W(o)[1].p;
            set_hdr(o, O_BLACKHOLE, 2);
            goto eval;
        case O_BLACKHOLE:
            runtime_error("<<loop>>");
            break;
        case O_STR: {
            /* Unpack one character: the object becomes the cons cell. */
            const char *s = (const char *)W(o)[0].c;
            unsigned long len = W(o)[1].u;
            if (len == 0) {
                set_hdr(o, O_IND, 2);
                W(o)[0].p = nullary(prog->con_nil);
                o = W(o)[0].p;
                continue;
            } else {
                const char *p = s;
                unsigned long c = utf8_next(&p);
                unsigned long used = (unsigned long)(p - s);
                Obj *head;
                Obj *tail;
                Obj *cell;
                root_push(&o);
                reserve(2 + 3 + 3);
                root_pop();
                head = c < 256 ? small_chars[c] : alloc(O_CHAR, 1);
                if (c >= 256) {
                    W(head)[0].u = c;
                }
                tail = alloc(O_STR, 2);
                W(tail)[0].c = p;
                W(tail)[1].u = len - used;
                cell = alloc(O_CON, 3);
                W(cell)[0].c = prog->con_cons;
                W(cell)[1].p = head;
                W(cell)[2].p = tail;
                set_hdr(o, O_IND, 2);
                W(o)[0].p = cell;
                reg_val = cell;
                goto ret;
            }
        }
        default:
            reg_val = o;
            goto ret;
        }
    }

ret:
    /* Return the value in reg_val to the frame on top of the stack. */
    for (;;) {
        Frame *f = &stack[sp - 1];
        switch (f->kind) {
        case K_STOP:
            sp--;
            return reg_val;
        case K_UPDATE: {
            Obj *t = f->a;
            set_hdr(t, O_IND, 2);
            W(t)[0].p = reg_val;
            W(t)[1].p = NULL;
            sp--;
            continue;
        }
        case K_CASE: {
            const Core *c = (const Core *)f->code;
            const CoreAlt *alt = select_alt(c, reg_val);
            Obj *env = f->b;
            sp--;
            if (alt == NULL) {
                runtime_error("case: no alternative matches");
            }
            if (alt->kind == ALT_CON && alt->con->arity > 0) {
                unsigned long n = (unsigned long)alt->con->arity;
                unsigned long i;
                Obj *frame;
                reg_env = env;
                reserve(2 + n);
                frame = alloc(O_ENV, 1 + n);
                W(frame)[0].p = reg_env;
                for (i = 0; i < n; i++) {
                    W(frame)[1 + i].p = W(reg_val)[1 + i].p;
                }
                reg_env = frame;
            } else {
                reg_env = env;
            }
            code = alt->body;
            goto eval;
        }
        case K_ARG: {
            /* Apply the value to the arguments on the stack. */
            Obj *fun = reg_val;
            Obj *inner;
            unsigned long have = 0; /* arguments already in a PAP */
            unsigned long arity;
            unsigned long avail = args_on_stack();
            unsigned long i;
            if (KIND(fun) == O_PAP) {
                inner = W(fun)[0].p;
                have = SIZE(fun) - 1;
            } else {
                inner = fun;
            }
            if (KIND(inner) == O_FUN) {
                arity = ((const Core *)W(inner)[0].c)->arity;
            } else if (KIND(inner) == O_PRIM) {
                arity = prim_of(inner)->arity;
            } else {
                runtime_error("a value that is not a function was applied");
                return NULL;
            }
            if (have + avail < arity) {
                /* Not enough arguments: a (bigger) partial application. */
                Obj *pap;
                reserve(2 + have + avail);
                fun = reg_val;
                inner = KIND(fun) == O_PAP ? W(fun)[0].p : fun;
                pap = alloc(O_PAP, 1 + have + avail);
                W(pap)[0].p = inner;
                for (i = 0; i < have; i++) {
                    W(pap)[1 + i].p = W(fun)[1 + i].p;
                }
                for (i = 0; i < avail; i++) {
                    W(pap)[1 + have + i].p = stack[sp - 1 - i].a;
                }
                sp -= avail;
                reg_val = pap;
                continue;
            }
            if (KIND(inner) == O_FUN) {
                const Core *lam;
                Obj *frame;
                reserve(2 + arity);
                fun = reg_val;
                inner = KIND(fun) == O_PAP ? W(fun)[0].p : fun;
                lam = (const Core *)W(inner)[0].c;
                frame = alloc(O_ENV, 1 + arity);
                W(frame)[0].p = W(inner)[1].p;
                for (i = 0; i < have; i++) {
                    W(frame)[1 + i].p = W(fun)[1 + i].p;
                }
                for (i = have; i < arity; i++) {
                    W(frame)[1 + i].p = stack[sp - 1 - (i - have)].a;
                }
                sp -= arity - have;
                reg_env = frame;
                code = lam->body;
                goto eval;
            } else {
                /* A primitive: collect the arguments, then evaluate the
                 * strict ones before the call. */
                const PrimInfo *info = prim_of(inner);
                Obj *args;
                unsigned long j;
                reserve(2 + arity);
                fun = reg_val;
                inner = KIND(fun) == O_PAP ? W(fun)[0].p : fun;
                args = alloc(O_PAP, 1 + arity);
                W(args)[0].p = inner;
                for (i = 0; i < have; i++) {
                    W(args)[1 + i].p = W(fun)[1 + i].p;
                }
                for (i = have; i < arity; i++) {
                    W(args)[1 + i].p = stack[sp - 1 - (i - have)].a;
                }
                sp -= arity - have;
                for (j = 0; j < arity; j++) {
                    if (info->strict & (1UL << j)) {
                        break;
                    }
                }
                if (j < arity) {
                    push(K_PRIM, args, NULL, info, j);
                    o = W(args)[1 + j].p;
                    goto enter;
                }
                push(K_PRIM, args, NULL, info, arity);
                o = call_prim(info);
                goto enter;
            }
        }
        case K_PRIM: {
            const PrimInfo *info = (const PrimInfo *)f->code;
            Obj *args = f->a;
            unsigned long j = f->n;
            W(args)[1 + j].p = reg_val;
            for (j++; j < info->arity; j++) {
                if (info->strict & (1UL << j)) {
                    break;
                }
            }
            if (j < info->arity) {
                f->n = j;
                o = W(args)[1 + j].p;
                goto enter;
            }
            f->n = info->arity;
            o = call_prim(info);
            goto enter;
        }
        case K_CATCH:
            /* The action finished without an exception. */
            sp--;
            continue;
        }
    }
}

/* --- Exceptions --------------------------------------------------------- */

/* The message of an exception that nothing catches. The evaluator
 * knows the exception types of the boot base by name. A SomeException
 * holds the Exception dictionary and the exception. */
static char *exception_message(Obj *exc)
{
    const ConInfo *con;
    exc = force(exc);
    if (KIND(exc) != O_CON) {
        return xstrdup("uncaught exception");
    }
    con = (const ConInfo *)W(exc)[0].c;
    if (str_eq(con->name, "SomeException") && con->arity == 2) {
        return exception_message(W(exc)[2].p);
    }
    if (str_eq(con->type->name, "ErrorCall") && con->arity == 1) {
        return force_string(W(exc)[1].p);
    }
    if (str_eq(con->name, "DivideByZero")) {
        return xstrdup("divide by zero");
    }
    return str_printf("uncaught exception: %s", con->name);
}

/* Pop the frames of the stack down to the K_STOP frame at base, after
 * a raise. At a K_CATCH frame, return the handler with its arguments
 * pushed, for the machine to enter. At the K_STOP frame, jump on to the
 * run that called this run (prev), or report the exception when there
 * is none. */
static Obj *unwind(size_t base, jmp_buf *prev)
{
    for (;;) {
        Frame *f = &stack[sp - 1];
        switch (f->kind) {
        case K_CATCH: {
            Obj *handler = f->a;
            Obj *world = f->b;
            sp--;
            push(K_ARG, world, NULL, NULL, 0);
            push(K_ARG, reg_exc, NULL, NULL, 0);
            reg_exc = NULL;
            return handler;
        }
        case K_UPDATE:
            /* The thunk is a black hole: make it a thunk again. */
            set_hdr(f->a, O_THUNK, 2);
            sp--;
            break;
        case K_STOP:
            if (sp - 1 != base) {
                runtime_error("the stack of an exception is not consistent");
            }
            sp--;
            cur_jump = prev;
            if (prev != NULL) {
                longjmp(*prev, 1);
            } else {
                Obj *exc = reg_exc;
                reg_exc = NULL;
                runtime_error(exception_message(exc));
            }
            return NULL;
        default:
            sp--;
            break;
        }
    }
}

/* Run the machine from an object until it returns to the K_STOP frame
 * that this call pushes. Returns the value in weak head normal form.
 * Every run is a target for a raise. */
static Obj *run(Obj *start)
{
    jmp_buf jump;
    jmp_buf *prev = cur_jump;
    size_t base = sp;
    size_t base_roots = nroots;
    Obj *result;
    push(K_STOP, NULL, NULL, NULL, 0);
    cur_jump = &jump;
    if (setjmp(jump) != 0) {
        /* A raise: the roots of the primitives that this run called are
         * gone. */
        nroots = base_roots;
        result = machine(unwind(base, prev));
    } else {
        result = machine(start);
    }
    cur_jump = prev;
    return result;
}

/* --- Setup and entry ---------------------------------------------------- */

static void init_small_values(void)
{
    long i;
    for (i = -16; i < 256; i++) {
        Obj *o = alloc_static(O_INT, 1);
        W(o)[0].i = i;
        small_ints[i + 16] = o;
    }
    for (i = 0; i < 256; i++) {
        Obj *o = alloc_static(O_CHAR, 1);
        W(o)[0].u = (unsigned long)i;
        small_chars[i] = o;
    }
}

/* One heap object for every global: a primitive, a closure, a nullary
 * constructor, or a thunk for a constant applicative form. */
/* Whether a global's body is built directly. A body that names other
 * globals becomes a thunk, because their objects may not exist yet. */
static int direct_global(const Core *body)
{
    switch (body->kind) {
    case CORE_INT:
    case CORE_CHAR:
    case CORE_STR:
    case CORE_LAM:
        return 1;
    case CORE_CON:
        return body->args.len == 0;
    default:
        return 0;
    }
}

static void init_globals(void)
{
    size_t i;
    unsigned long words = 0;
    for (i = 0; i < prog->globals.len; i++) {
        const Global *g = (const Global *)prog->globals.items[i];
        if (g->prim == NULL) {
            words += direct_global(g->body) ? atom_size(g->body) : 3;
        }
    }
    reserve(words);
    for (i = 0; i < prog->globals.len; i++) {
        Global *g = (Global *)prog->globals.items[i];
        if (g->prim != NULL) {
            Obj *o = alloc_static(O_PRIM, 1);
            W(o)[0].c = g->prim;
            g->value = o;
        } else if (direct_global(g->body)) {
            g->value = atom(g->body, NULL);
        } else {
            Obj *o = alloc(O_THUNK, 2);
            W(o)[0].c = g->body;
            W(o)[1].p = NULL;
            g->value = o;
        }
    }
}

char *eval_run(CoreProgram *program)
{
    Obj *result;
    const char *heap_env = getenv("AIHC_BOOT_HEAP");
    prog = program;
    /* The initial heap in words. AIHC_BOOT_HEAP overrides it, so the
     * tests can force frequent collections. */
    heap_size = 1UL << 20;
    if (heap_env != NULL && atol(heap_env) >= 1024) {
        heap_size = (unsigned long)atol(heap_env);
    }
    heap = (Word *)xmalloc(heap_size * sizeof(Word));
    heap_end = heap + heap_size;
    hp = heap;
    init_small_values();
    init_globals();
    /* main is an IO action: a function from the world to `IORes a`. The
     * world is the unit value. */
    {
        Vec none;
        Core *app;
        Obj *start;
        vec_init(&none);
        app = core_app1(core_global(prog->main), core_con(prog->con_unit, &none));
        reserve(3);
        start = alloc(O_THUNK, 2);
        W(start)[0].c = app;
        W(start)[1].p = NULL;
        result = run(start);
    }
    fflush(stdout);
    if (KIND(result) != O_CON || (const ConInfo *)W(result)[0].c != prog->con_iores) {
        return xstrdup("main did not return an IO result");
    }
    return NULL;
}
