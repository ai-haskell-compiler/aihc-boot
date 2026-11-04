/* The C preprocessor step: see cpp.h.
 *
 * aihc-boot defines the macros that GHC 9.12 and Cabal define:
 * __GLASGOW_HASKELL__, MIN_VERSION_GLASGOW_HASKELL, the platform macros
 * (linux_HOST_OS and the others), and VERSION_pkg and MIN_VERSION_pkg for
 * the package of the module and its dependencies. The headers of GHC
 * that the vendored code includes, such as MachDeps.h, are in
 * cpp/include. The cpp-options and the include-dirs of the package come
 * from its .cabal file.
 */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cpp.h"
#include "manifest.h"
#include "strmap.h"
#include "sys.h"
#include "util.h"

#define CPP_DIR "build/cpp"
#define INCLUDE_DIR "cpp/include"

/* The platform of aihc-boot, which runs the C code of the packages. */
#if defined(__APPLE__)
#define HOST_OS "darwin"
#elif defined(__linux__)
#define HOST_OS "linux"
#elif defined(__FreeBSD__)
#define HOST_OS "freebsd"
#else
#define HOST_OS "unknown"
#endif

#if defined(__x86_64__) || defined(__amd64__)
#define HOST_ARCH "x86_64"
#elif defined(__aarch64__)
#define HOST_ARCH "aarch64"
#elif defined(__i386__)
#define HOST_ARCH "i386"
#else
#define HOST_ARCH "unknown"
#endif

/* The packages that come with GHC 9.12 and that are not vendored, or
 * whose vendored copy is the boot replacement. */
static const char *const ghc_packages[][2] = {
    {"base", "4.21.1.0"},
    {"ghc-prim", "0.13.0"},
    {"ghc-bignum", "1.3"},
    {"ghc-internal", "9.1203.0"},
    {"template-haskell", "2.23.0.0"},
};

/* path -> the path of its source text (see source_path). */
static StrMap sources;
static int sources_init;
/* The vendored packages, read on first use. */
static StrMap packages;
static int packages_init;
/* package name -> the path of its macro file. */
static StrMap macro_files;

static const StrMap *vendored(void)
{
    if (!packages_init) {
        packages = vendored_packages();
        packages_init = 1;
    }
    return &packages;
}

/* Whether a LANGUAGE pragma of the text turns on CPP. */
static int has_cpp_pragma(const char *text)
{
    const char *p = text;
    while ((p = strstr(p, "{-#")) != NULL) {
        const char *end = strstr(p, "#-}");
        const char *q = p + 3;
        if (end == NULL) {
            return 0;
        }
        while (q < end && (*q == ' ' || *q == '\t' || *q == '\n' || *q == '\r')) {
            q++;
        }
        if (end - q >= 8 && (strncmp(q, "LANGUAGE", 8) == 0 || strncmp(q, "language", 8) == 0)) {
            q += 8;
            while (q < end) {
                const char *start;
                while (q < end && (*q == ' ' || *q == '\t' || *q == '\n' || *q == '\r'
                                   || *q == ',')) {
                    q++;
                }
                start = q;
                while (q < end && *q != ' ' && *q != '\t' && *q != '\n' && *q != '\r'
                       && *q != ',') {
                    q++;
                }
                if (q - start == 3 && strncmp(start, "CPP", 3) == 0) {
                    return 1;
                }
            }
        }
        p = end + 3;
    }
    return 0;
}

/* The vendored package of a path under vendor/, or NULL. */
static char *package_of(const char *path)
{
    const char *rest;
    const char *slash;
    char *name;
    if (!str_starts_with(path, "vendor/")) {
        return NULL;
    }
    rest = path + strlen("vendor/");
    slash = strchr(rest, '/');
    if (slash == NULL) {
        return NULL;
    }
    name = xstrndup(rest, (size_t)(slash - rest));
    return strmap_has(vendored(), name) ? name : NULL;
}

static const char *ghc_package_version(const char *name)
{
    size_t i;
    for (i = 0; i < sizeof ghc_packages / sizeof ghc_packages[0]; i++) {
        if (str_eq(ghc_packages[i][0], name)) {
            return ghc_packages[i][1];
        }
    }
    return NULL;
}

/* The version of a package, or NULL when aihc-boot does not know it. */
static const char *package_version(const char *name)
{
    const char *v = ghc_package_version(name);
    const CabalPackage *cabal;
    if (v != NULL) {
        return v;
    }
    cabal = (const CabalPackage *)strmap_get(vendored(), name);
    return cabal != NULL ? cabal->version : NULL;
}

/* VERSION_pkg and MIN_VERSION_pkg, as Cabal writes them. */
static void define_version(StrBuf *b, const char *name, const char *version)
{
    unsigned long v[3];
    const char *p = version;
    char *macro = xstrdup(name);
    char *q;
    size_t i;
    for (q = macro; *q != '\0'; q++) {
        if (*q == '-') {
            *q = '_';
        }
    }
    for (i = 0; i < 3; i++) {
        v[i] = 0;
        while (*p >= '0' && *p <= '9') {
            v[i] = v[i] * 10 + (unsigned long)(*p - '0');
            p++;
        }
        if (*p == '.') {
            p++;
        }
    }
    sb_printf(b, "#define VERSION_%s \"%s\"\n", macro, version);
    sb_printf(b,
              "#define MIN_VERSION_%s(major1,major2,minor) ("
              "(major1) < %lu || (major1) == %lu && (major2) < %lu || "
              "(major1) == %lu && (major2) == %lu && (minor) <= %lu)\n",
              macro, v[0], v[0], v[1], v[0], v[1], v[2]);
}

/* The macro file for the modules of a package (NULL: a file outside the
 * vendored packages). Returns NULL and sets *err when it cannot write the
 * file. */
static const char *macro_file(const char *package, char **err)
{
    const char *key = package != NULL ? package : "";
    const char *done = (const char *)strmap_get(&macro_files, key);
    StrBuf b;
    StrMap names;
    char *path;
    size_t i;
    if (done != NULL) {
        return done;
    }
    sb_init(&b);
    sb_puts(&b, "#define __GLASGOW_HASKELL__ 912\n"
                "#define __GLASGOW_HASKELL_FULL_VERSION__ \"9.12.3\"\n"
                "#define __GLASGOW_HASKELL_PATCHLEVEL1__ 3\n"
                "#define MIN_VERSION_GLASGOW_HASKELL(ma,mi,pl1,pl2) ("
                "(ma)*100+(mi) < __GLASGOW_HASKELL__ || "
                "(ma)*100+(mi) == __GLASGOW_HASKELL__ && (pl1) < 3 || "
                "(ma)*100+(mi) == __GLASGOW_HASKELL__ && (pl1) == 3 && (pl2) <= 0)\n");
    sb_printf(&b, "#define %s_HOST_OS 1\n#define %s_BUILD_OS 1\n", HOST_OS, HOST_OS);
    sb_printf(&b, "#define %s_HOST_ARCH 1\n#define %s_BUILD_ARCH 1\n", HOST_ARCH, HOST_ARCH);
    /* The package itself and its dependencies. Every module can use base. */
    strmap_init(&names);
    strmap_put(&names, "base", NULL);
    if (package != NULL) {
        const CabalPackage *cabal = (const CabalPackage *)strmap_get(vendored(), package);
        strmap_put(&names, package, NULL);
        for (i = 0; i < cabal->depends.len; i++) {
            strmap_put(&names, cabal->depends.keys[i], NULL);
        }
        if (cabal->version != NULL) {
            sb_printf(&b, "#define CURRENT_PACKAGE_VERSION \"%s\"\n", cabal->version);
        }
    }
    for (i = 0; i < names.len; i++) {
        const char *version = package_version(names.keys[i]);
        if (version != NULL) {
            define_version(&b, names.keys[i], version);
        }
    }
    path = str_printf("%s/macros/%s.h", CPP_DIR, package != NULL ? package : "-");
    errno = 0;
    if (sys_create_dir_all(CPP_DIR "/macros") != 0 || write_file(path, b.s, b.len) != 0) {
        *err = str_printf("cannot write %s: %s", path, os_error(errno != 0 ? errno : EIO));
        return NULL;
    }
    strmap_put(&macro_files, key, path);
    return path;
}

static char *cpp_output_path(const char *path)
{
    while (*path == '/') {
        path++;
    }
    return path_join(CPP_DIR, path);
}

/* Remove the line markers (`# 12 "file"`) from the output of the
 * preprocessor. Blank lines keep the lines of the module itself at their
 * place in the file, so positions in messages stay right. lines is the
 * number of lines of the module. */
static char *strip_line_markers(const char *text, const char *path, unsigned long lines)
{
    StrBuf b;
    const char *p = text;
    unsigned long line = 0; /* lines written so far */
    int in_module = 1;
    sb_init(&b);
    while (*p != '\0') {
        const char *end = strchr(p, '\n');
        size_t n = end != NULL ? (size_t)(end - p) : strlen(p);
        if (n >= 3 && p[0] == '#' && p[1] == ' ' && p[2] >= '0' && p[2] <= '9') {
            const char *q = p + 2;
            unsigned long target = 0;
            const char *name;
            const char *name_end;
            while (*q >= '0' && *q <= '9') {
                target = target * 10 + (unsigned long)(*q - '0');
                q++;
            }
            name = strchr(q, '"');
            name_end = name != NULL ? strchr(name + 1, '"') : NULL;
            in_module = name != NULL && name_end != NULL
                        && (size_t)(name_end - name - 1) == strlen(path)
                        && strncmp(name + 1, path, strlen(path)) == 0;
            while (in_module && line + 1 < target) {
                sb_putc(&b, '\n');
                line++;
            }
        } else if (in_module || strspn(p, " \t") < n) {
            /* A blank line of a header has no code: drop it, so the
             * lines of the module stay at their place. */
            sb_putn(&b, p, n);
            sb_putc(&b, '\n');
            line++;
        }
        if (end == NULL) {
            break;
        }
        p = end + 1;
    }
    /* The preprocessor adds blank lines at the end. Keep the end of the
     * module at its line too. */
    while (line > lines && b.len >= 2 && b.s[b.len - 1] == '\n' && b.s[b.len - 2] == '\n') {
        b.len--;
        b.s[b.len] = '\0';
        line--;
    }
    while (line < lines) {
        sb_putc(&b, '\n');
        line++;
    }
    return sb_take(&b);
}

static char *first_line(const char *s)
{
    const char *nl = strchr(s, '\n');
    return nl != NULL ? xstrndup(s, (size_t)(nl - s)) : xstrdup(s);
}

/* Run clang on path. Returns the text, or NULL with *err set. */
static char *preprocess(const char *path, const char *package, unsigned long lines,
                        char **err)
{
    const char *env = getenv("AIHC_CPP");
    char *command = env != NULL ? xstrdup(env) : sys_find_in_path("clang");
    const CabalPackage *cabal = NULL;
    const char *macros;
    char *out_path = cpp_output_path(path);
    char *raw_path = str_concat(out_path, ".raw");
    char *err_path = str_printf("%s/cpp-%lu.err", CPP_DIR, sys_pid());
    char *dir = path_parent(out_path);
    Vec argv;
    size_t i;
    int status;
    int e;
    char *raw;
    char *text;
    size_t n;
    if (command == NULL) {
        *err = xstrdup("cpp: clang not found: set AIHC_CPP or add clang to PATH");
        return NULL;
    }
    if ((macros = macro_file(package, err)) == NULL) {
        return NULL;
    }
    if (dir != NULL && (e = sys_create_dir_all(dir)) != 0) {
        *err = str_printf("cpp: cannot create %s: %s", dir, os_error(e));
        return NULL;
    }
    if (package != NULL) {
        cabal = (const CabalPackage *)strmap_get(vendored(), package);
    }
    vec_init(&argv);
    vec_push(&argv, command);
    /* The flags of the "Haskell CPP flags" setting of GHC 9.12, and the
     * language that GHC selects for Haskell sources. */
    vec_push(&argv, "-E");
    vec_push(&argv, "-undef");
    vec_push(&argv, "-traditional");
    vec_push(&argv, "-Wno-invalid-pp-token");
    vec_push(&argv, "-Wno-unicode");
    vec_push(&argv, "-Wno-trigraphs");
    vec_push(&argv, "-x");
    vec_push(&argv, "assembler-with-cpp");
    vec_push(&argv, "-I");
    vec_push(&argv, INCLUDE_DIR);
    if (cabal != NULL) {
        for (i = 0; i < cabal->include_dirs.len; i++) {
            vec_push(&argv, "-I");
            vec_push(&argv, path_join(package_dir(package),
                                      (const char *)cabal->include_dirs.items[i]));
        }
        for (i = 0; i < cabal->cpp_options.len; i++) {
            vec_push(&argv, cabal->cpp_options.items[i]);
        }
    }
    vec_push(&argv, "-include");
    vec_push(&argv, (char *)macros);
    vec_push(&argv, (char *)path);
    vec_push(&argv, "-o");
    vec_push(&argv, raw_path);
    vec_push(&argv, NULL);
    e = sys_run((char *const *)argv.items, NULL, NULL, err_path, &status);
    if (e != 0) {
        remove(err_path);
        *err = str_printf("cpp: cannot run %s: %s", command, os_error(e));
        return NULL;
    }
    if (status != 0) {
        char *msg = read_file(err_path, &n);
        remove(err_path);
        *err = str_printf("cpp: %s", first_line(msg != NULL ? msg : ""));
        return NULL;
    }
    remove(err_path);
    raw = read_text(raw_path, NULL, err);
    remove(raw_path);
    if (raw == NULL) {
        return NULL;
    }
    text = strip_line_markers(raw, path, lines);
    errno = 0;
    if (write_file(out_path, text, strlen(text)) != 0) {
        *err = str_printf("cpp: cannot write %s: %s", out_path,
                          os_error(errno != 0 ? errno : EIO));
        return NULL;
    }
    strmap_put(&sources, xstrdup(path), out_path);
    return text;
}

char *source_text(const char *path, size_t *len, char **err)
{
    const char *done;
    char *text;
    char *package;
    const CabalPackage *cabal;
    if (!sources_init) {
        strmap_init(&sources);
        strmap_init(&macro_files);
        sources_init = 1;
    }
    done = (const char *)strmap_get(&sources, path);
    if (done != NULL) {
        return read_text(done, len, err);
    }
    text = read_text(path, len, err);
    if (text == NULL) {
        return NULL;
    }
    package = package_of(path);
    cabal = package != NULL ? (const CabalPackage *)strmap_get(vendored(), package) : NULL;
    if (!has_cpp_pragma(text)
        && !(cabal != NULL && vec_has_str(&cabal->extensions, "CPP"))) {
        strmap_put(&sources, xstrdup(path), xstrdup(path));
        return text;
    }
    {
        unsigned long lines = 0;
        const char *q;
        for (q = text; *q != '\0'; q++) {
            if (*q == '\n') {
                lines++;
            }
        }
        if (q > text && q[-1] != '\n') {
            lines++;
        }
        text = preprocess(path, package, lines, err);
    }
    if (text != NULL && len != NULL) {
        *len = strlen(text);
    }
    return text;
}

const char *source_path(const char *path)
{
    const char *done;
    char *err;
    if (sources_init && (done = (const char *)strmap_get(&sources, path)) != NULL) {
        return done;
    }
    if (source_text(path, NULL, &err) == NULL) {
        return path;
    }
    done = (const char *)strmap_get(&sources, path);
    return done != NULL ? done : path;
}
