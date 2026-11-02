/* The vendored packages: what `vendor/NAME/NAME.cabal` says about a
 * package, and the manifest that `tools/resolve-oracle` reads.
 *
 * The `.cabal` reader is line based and reads three fields of every
 * stanza: `build-depends`, `default-language` and `default-extensions`.
 * It takes the union over the stanzas, so a dependency of a test suite
 * counts as well. That is harmless: a dependency is only important here
 * when it is vendored.
 */
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "manifest.h"
#include "sys.h"

/* --- Paths and files ---------------------------------------------------- */

char *path_join(const char *a, const char *b)
{
    size_t n = strlen(a);
    if (b[0] == '/' || n == 0) {
        return xstrdup(b);
    }
    if (a[n - 1] == '/') {
        return str_concat(a, b);
    }
    return str_concat3(a, "/", b);
}

int path_cmp(const char *a, const char *b)
{
    for (;;) {
        size_t na;
        size_t nb;
        size_t n;
        int c;
        /* Rust skips empty components. A leading `/` is the root
         * component, which comes before every name. */
        while (*a == '/' && a[1] == '/') {
            a++;
        }
        while (*b == '/' && b[1] == '/') {
            b++;
        }
        if (*a == '\0' || *b == '\0') {
            return (*a != '\0') - (*b != '\0');
        }
        na = strcspn(a, "/");
        nb = strcspn(b, "/");
        if (na == 0 || nb == 0) {
            /* A root component. */
            if (na != nb) {
                return na == 0 ? -1 : 1;
            }
            a++;
            b++;
            continue;
        }
        n = na < nb ? na : nb;
        c = memcmp(a, b, n);
        if (c != 0) {
            return c < 0 ? -1 : 1;
        }
        if (na != nb) {
            return na < nb ? -1 : 1;
        }
        a += na;
        b += nb;
        while (*a == '/') {
            a++;
        }
        while (*b == '/') {
            b++;
        }
    }
}

/* The last component of a path. */
static const char *file_name(const char *path)
{
    const char *slash = strrchr(path, '/');
    return slash ? slash + 1 : path;
}

char *path_extension(const char *path)
{
    const char *name = file_name(path);
    const char *dot;
    if (str_eq(name, "..")) {
        return NULL;
    }
    dot = strrchr(name, '.');
    if (dot == NULL || dot == name) {
        return NULL;
    }
    return xstrdup(dot + 1);
}

/* The file name without its extension, as Rust's Path::file_stem. */
static char *file_stem(const char *path)
{
    const char *name = file_name(path);
    const char *dot;
    if (str_eq(name, "..")) {
        return xstrdup(name);
    }
    dot = strrchr(name, '.');
    if (dot == NULL || dot == name) {
        return xstrdup(name);
    }
    return xstrndup(name, (size_t)(dot - name));
}

char *path_parent(const char *path)
{
    const char *slash = strrchr(path, '/');
    if (slash == NULL) {
        return NULL;
    }
    return xstrndup(path, (size_t)(slash - path));
}

char *os_error(int err)
{
    return str_printf("%s (os error %d)", strerror(err), err);
}

/* Whether s[0..n) is valid UTF-8, as Rust's str::from_utf8 decides. */
static int utf8_valid(const unsigned char *s, size_t n)
{
    size_t i = 0;
    while (i < n) {
        unsigned char c = s[i];
        size_t len;
        unsigned long cp;
        size_t k;
        if (c < 0x80) {
            i++;
            continue;
        } else if (c >= 0xC2 && c <= 0xDF) {
            len = 2;
            cp = c & 0x1F;
        } else if (c >= 0xE0 && c <= 0xEF) {
            len = 3;
            cp = c & 0x0F;
        } else if (c >= 0xF0 && c <= 0xF4) {
            len = 4;
            cp = c & 0x07;
        } else {
            return 0;
        }
        if (i + len > n) {
            return 0;
        }
        for (k = 1; k < len; k++) {
            if ((s[i + k] & 0xC0) != 0x80) {
                return 0;
            }
            cp = (cp << 6) | (s[i + k] & 0x3F);
        }
        if ((len == 3 && cp < 0x800) || (len == 4 && cp < 0x10000) || cp > 0x10FFFF
            || (cp >= 0xD800 && cp <= 0xDFFF)) {
            return 0;
        }
        i += len;
    }
    return 1;
}

char *read_text(const char *path, size_t *len, char **err)
{
    size_t n;
    char *text;
    errno = 0;
    text = read_file(path, &n);
    if (text == NULL) {
        *err = os_error(errno != 0 ? errno : EIO);
        return NULL;
    }
    if (!utf8_valid((const unsigned char *)text, n)) {
        *err = xstrdup("stream did not contain valid UTF-8");
        return NULL;
    }
    if (len != NULL) {
        *len = n;
    }
    return text;
}

char *utf8_lossy(const char *s, size_t n)
{
    unsigned long *cps;
    size_t count = utf8_decode(s, n, &cps);
    size_t i;
    StrBuf b;
    sb_init(&b);
    for (i = 0; i < count; i++) {
        sb_put_utf8(&b, cps[i]);
    }
    free(cps);
    return sb_take(&b);
}

/* --- White space, as Rust's char::is_whitespace -------------------------- */

/* The number of bytes of white space at the start of s. */
static size_t ws_start(const char *s)
{
    const char *p = s;
    for (;;) {
        const char *q = p;
        if (*q == '\0' || !uc_is_whitespace(utf8_next(&q))) {
            return (size_t)(p - s);
        }
        p = q;
    }
}

/* The length of s without the white space at its end. */
static size_t len_without_ws_end(const char *s)
{
    const char *p = s;
    size_t end = 0;
    while (*p != '\0') {
        unsigned long c = utf8_next(&p);
        if (!uc_is_whitespace(c)) {
            end = (size_t)(p - s);
        }
    }
    return end;
}

static char *trim_end(const char *s)
{
    return xstrndup(s, len_without_ws_end(s));
}

static int has_ws(const char *s)
{
    while (*s != '\0') {
        if (uc_is_whitespace(utf8_next(&s))) {
            return 1;
        }
    }
    return 0;
}

/* --- .cabal files ------------------------------------------------------- */

typedef struct {
    char *name;
    StrBuf value;
} CabalField;

/* Every `name: value` field of a `.cabal` file (CabalField *), in order,
 * with the field name in lower case. A field's value continues on the
 * lines that are indented more than the field. The reader removes
 * comments (`--`). */
static Vec fields(const char *text)
{
    Vec out;
    Vec lines = str_lines(text);
    int has_indent = 0;
    size_t field_indent = 0;
    size_t i;
    vec_init(&out);
    for (i = 0; i < lines.len; i++) {
        const char *raw = (const char *)lines.items[i];
        const char *comment = strstr(raw, "--");
        char *before = comment ? xstrndup(raw, (size_t)(comment - raw)) : xstrdup(raw);
        char *line = trim_end(before);
        char *trimmed = str_trim(line);
        size_t indent;
        char *colon;
        if (trimmed[0] == '\0') {
            continue;
        }
        indent = ws_start(line);
        if (has_indent) {
            if (indent > field_indent) {
                CabalField *last = (CabalField *)vec_last(&out);
                sb_putc(&last->value, ' ');
                sb_puts(&last->value, trimmed);
                continue;
            }
            has_indent = 0;
        }
        colon = strchr(trimmed, ':');
        if (colon != NULL) {
            char *name_raw = xstrndup(trimmed, (size_t)(colon - trimmed));
            char *name = str_trim(name_raw);
            /* A stanza header such as `executable aihc` has no colon; a
             * field name has no spaces. */
            if (name[0] != '\0' && !has_ws(name)) {
                CabalField *f = (CabalField *)xmalloc(sizeof *f);
                char *p;
                for (p = name; *p != '\0'; p++) {
                    if (*p >= 'A' && *p <= 'Z') {
                        *p = (char)(*p - 'A' + 'a');
                    }
                }
                f->name = name;
                sb_init(&f->value);
                sb_puts(&f->value, str_trim(colon + 1));
                vec_push(&out, f);
                has_indent = 1;
                field_indent = indent;
            }
        }
    }
    return out;
}

static int is_dep_char(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
           || c == '-';
}

CabalPackage *cabal_parse(const char *text)
{
    CabalPackage *package = (CabalPackage *)xmalloc(sizeof *package);
    Vec fs = fields(text);
    size_t i;
    size_t j;
    package->language = NULL;
    vec_init(&package->extensions);
    strmap_init(&package->depends);
    for (i = 0; i < fs.len; i++) {
        const CabalField *f = (const CabalField *)fs.items[i];
        const char *value = f->value.s ? f->value.s : "";
        if (str_eq(f->name, "build-depends")) {
            Vec items = str_split(value, ',');
            for (j = 0; j < items.len; j++) {
                char *item = str_trim((const char *)items.items[j]);
                size_t n = 0;
                while (is_dep_char(item[n])) {
                    n++;
                }
                if (n > 0) {
                    strmap_put(&package->depends, xstrndup(item, n), NULL);
                }
            }
        } else if (str_eq(f->name, "default-language")) {
            char *v = str_trim(value);
            if (package->language == NULL && v[0] != '\0') {
                package->language = v;
            }
        } else if (str_eq(f->name, "default-extensions")) {
            /* Split at `,` and at white space. */
            const char *p = value;
            const char *start = value;
            for (;;) {
                const char *here = p;
                unsigned long c = *p == '\0' ? 0 : utf8_next(&p);
                if (c == 0 || c == ',' || uc_is_whitespace(c)) {
                    char *ext = xstrndup(start, (size_t)(here - start));
                    if (ext[0] != '\0' && !vec_has_str(&package->extensions, ext)) {
                        vec_push(&package->extensions, ext);
                    }
                    if (c == 0) {
                        break;
                    }
                    start = p;
                }
            }
        }
    }
    return package;
}

/* Compare two paths, for vec_sort. The items are char *. */
static int path_ptr_cmp(const void *a, const void *b)
{
    return path_cmp(*(const char *const *)a, *(const char *const *)b);
}

CabalPackage *cabal_read(const char *pkg_dir)
{
    Vec names;
    Vec cabal_files;
    size_t i;
    char *text;
    char *err;
    if (sys_list_dir(pkg_dir, &names) != 0) {
        return NULL;
    }
    vec_init(&cabal_files);
    for (i = 0; i < names.len; i++) {
        char *path = path_join(pkg_dir, (const char *)names.items[i]);
        char *ext = path_extension(path);
        if (ext != NULL && str_eq(ext, "cabal")) {
            vec_push(&cabal_files, path);
        }
    }
    if (cabal_files.len == 0) {
        return NULL;
    }
    vec_sort(&cabal_files, path_ptr_cmp);
    text = read_text((const char *)cabal_files.items[0], NULL, &err);
    if (text == NULL) {
        return NULL;
    }
    return cabal_parse(text);
}

Vec cabal_ghc_flags(const CabalPackage *p)
{
    Vec out;
    size_t i;
    vec_init(&out);
    if (p->language != NULL) {
        vec_push(&out, str_concat("-X", p->language));
    }
    for (i = 0; i < p->extensions.len; i++) {
        vec_push(&out, str_concat("-X", (const char *)p->extensions.items[i]));
    }
    return out;
}

/* --- Packages ----------------------------------------------------------- */

static void walk(const char *dir, int at_root, Vec *out)
{
    Vec names;
    size_t i;
    if (sys_list_dir(dir, &names) != 0) {
        return;
    }
    for (i = 0; i < names.len; i++) {
        char *path = path_join(dir, (const char *)names.items[i]);
        char *ext;
        int is_haskell;
        int is_setup;
        if (sys_is_dir(path)) {
            walk(path, 0, out);
            continue;
        }
        ext = path_extension(path);
        is_haskell = ext != NULL
                     && (str_eq(ext, "hs") || str_eq(ext, "lhs") || str_eq(ext, "hsc")
                         || str_eq(ext, "hs-boot"));
        is_setup = at_root && str_eq(file_stem(path), "Setup");
        if (is_haskell && !is_setup) {
            vec_push(out, path);
        }
    }
}

Vec haskell_files(const char *pkg_dir)
{
    Vec files;
    vec_init(&files);
    walk(pkg_dir, 1, &files);
    vec_sort(&files, path_ptr_cmp);
    return files;
}

char *package_dir(const char *name)
{
    return path_join("vendor", name);
}

StrMap vendored_packages(void)
{
    StrMap packages;
    Vec names;
    size_t i;
    strmap_init(&packages);
    if (sys_list_dir("vendor", &names) != 0) {
        return packages;
    }
    for (i = 0; i < names.len; i++) {
        const char *name = (const char *)names.items[i];
        CabalPackage *package = cabal_read(path_join("vendor", name));
        if (package != NULL) {
            strmap_put(&packages, name, package);
        }
    }
    return packages;
}

static char *visit(const char *name, const StrMap *packages, Vec *done, Vec *active)
{
    const CabalPackage *package;
    size_t i;
    if (vec_has_str(done, name)) {
        return NULL;
    }
    if (vec_has_str(active, name)) {
        StrBuf b;
        vec_push(active, (char *)name);
        sb_init(&b);
        sb_puts(&b, "dependency cycle: ");
        for (i = 0; i < active->len; i++) {
            if (i > 0) {
                sb_puts(&b, " -> ");
            }
            sb_puts(&b, (const char *)active->items[i]);
        }
        return sb_take(&b);
    }
    vec_push(active, (char *)name);
    package = (const CabalPackage *)strmap_get(packages, name);
    for (i = 0; i < package->depends.len; i++) {
        const char *dep = package->depends.keys[i];
        if (strmap_has(packages, dep) && !str_eq(dep, name)) {
            char *e = visit(dep, packages, done, active);
            if (e != NULL) {
                return e;
            }
        }
    }
    vec_pop(active);
    vec_push(done, (char *)name);
    return NULL;
}

char *package_order(const char *target, const StrMap *packages, Vec *out)
{
    Vec active;
    vec_init(out);
    vec_init(&active);
    if (!strmap_has(packages, target)) {
        return str_printf("vendor/%s is not a vendored package", target);
    }
    return visit(target, packages, out, &active);
}

char *manifest(const char *target, char **out)
{
    StrMap packages = vendored_packages();
    Vec order;
    Vec builtins;
    StrBuf b;
    size_t i;
    size_t j;
    char *e = package_order(target, &packages, &order);
    if (e != NULL) {
        return e;
    }
    sb_init(&b);
    builtins = builtin_modules(&order);
    for (i = 0; i < builtins.len; i++) {
        sb_printf(&b, "builtin %s\n", (const char *)builtins.items[i]);
    }
    for (i = 0; i < order.len; i++) {
        const char *name = (const char *)order.items[i];
        const CabalPackage *package = (const CabalPackage *)strmap_get(&packages, name);
        Vec files;
        sb_printf(&b, "package %s\n", name);
        if (package->language != NULL) {
            sb_printf(&b, "language %s\n", package->language);
        }
        for (j = 0; j < package->extensions.len; j++) {
            sb_printf(&b, "extension %s\n", (const char *)package->extensions.items[j]);
        }
        files = haskell_files(package_dir(name));
        for (j = 0; j < files.len; j++) {
            sb_printf(&b, "module %s\n", (const char *)files.items[j]);
        }
    }
    sb_printf(&b, "report %s\n", target);
    *out = sb_take(&b);
    return NULL;
}

char *program(const char *target, Program **out, Vec *packages_out)
{
    StrMap packages = vendored_packages();
    Vec order;
    Vec builtins;
    Program *p;
    size_t i;
    size_t j;
    char *e = package_order(target, &packages, &order);
    if (e != NULL) {
        return e;
    }
    builtins = builtin_modules(&order);
    p = program_new(&builtins);
    if (packages_out != NULL) {
        vec_init(packages_out);
    }
    for (i = 0; i < order.len; i++) {
        const char *name = (const char *)order.items[i];
        const CabalPackage *cabal = (const CabalPackage *)strmap_get(&packages, name);
        Vec files = haskell_files(package_dir(name));
        Package *pkg = (Package *)xcalloc(1, sizeof *pkg);
        pkg->name = xstrdup(name);
        pkg->extensions = cabal->extensions;
        vec_init(&pkg->modules);
        for (j = 0; j < files.len; j++) {
            const char *file = (const char *)files.items[j];
            size_t len;
            char *err;
            char *src = read_text(file, &len, &err);
            SyntaxError serr;
            Module *module;
            SourceModule *source;
            if (src == NULL) {
                continue;
            }
            module = parse_source(src, len, &serr);
            if (module == NULL) {
                continue;
            }
            source = (SourceModule *)xmalloc(sizeof *source);
            source->path = xstrdup(file);
            source->module = module;
            vec_push(&pkg->modules, source);
        }
        program_add_package(p, pkg);
        if (packages_out != NULL) {
            vec_push(packages_out, pkg);
        }
    }
    *out = p;
    return NULL;
}
