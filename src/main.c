/* The `aihc-boot` command.
 *
 * `docs/PLAN.md` ("Compiler interface") defines the contract between this
 * program and `scripts/progress.py`. The commands:
 *
 * - `aihc-boot check --stage STAGE --package NAME`: check every module
 *   of `vendor/NAME/` up to STAGE and print one JSON object per module.
 * - `aihc-boot manifest --package NAME`: print the manifest that
 *   `tools/resolve-oracle` reads for the package. For debugging.
 * - `aihc-boot dump --stage resolve --package NAME`: print our
 *   resolution records of every module of the package. For debugging.
 * - `aihc-boot oracle --stage resolve --package NAME`: run the oracle on
 *   the package and print its records in the same form, so that `diff`
 *   can compare the two outputs. For debugging.
 * - `aihc-boot lex FILE`: print the tokens of one file, after layout.
 *   For debugging.
 * - `aihc-boot parse FILE`: print the syntax tree of one file. For
 *   debugging.
 * - `aihc-boot print FILE`: parse one file and print it back as source.
 *   This is what `check` sends to the reference parser.
 * - `aihc-boot run FILE.hs`: run a program against the boot `base`.
 *   See "Eval strategy" in docs/PLAN.md.
 * - `aihc-boot core FILE.hs`: print the Core of the program's own
 *   module. For debugging and the golden tests.
 *
 * Each command returns NULL on success, or an error message.
 */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "desugar.h"
#include "eval.h"
#include "manifest.h"
#include "cpp.h"
#include "resolve.h"
#include "syntax.h"
#include "sys.h"
#include "tc.h"
#include "util.h"

static const char USAGE[] =
    "usage:\n"
    "  aihc-boot check --stage {parse|resolve|typecheck} --package NAME\n"
    "  aihc-boot manifest --package NAME\n"
    "  aihc-boot dump --stage resolve --package NAME\n"
    "  aihc-boot oracle --stage resolve --package NAME\n"
    "  aihc-boot lex FILE\n"
    "  aihc-boot parse FILE\n"
    "  aihc-boot print FILE\n"
    "  aihc-boot run FILE.hs\n"
    "  aihc-boot run vendor/PKG/.../Main.hs\n"
    "  aihc-boot core [--all] FILE.hs\n";

/* The directory for the files that aihc-boot writes. */
#define BUILD_DIR "build"

/* --- Output ------------------------------------------------------------- */

/* Write s to stdout and flush it. Returns NULL, or the error message of a
 * failed write. */
static char *out_write(const char *s, size_t n)
{
    errno = 0;
    if (n > 0 && fwrite(s, 1, n, stdout) != n) {
        return os_error(errno != 0 ? errno : EIO);
    }
    if (fflush(stdout) != 0 || ferror(stdout)) {
        return os_error(errno != 0 ? errno : EIO);
    }
    return NULL;
}

static char *out_str(const char *s)
{
    return out_write(s, strlen(s));
}

/* --- Arguments ---------------------------------------------------------- */

/* Take the value of `--name` out of an argument list (char *). */
static char *take_option(Vec *args, const char *name, char **value)
{
    size_t i;
    for (i = 0; i < args->len; i++) {
        if (str_eq((const char *)args->items[i], name)) {
            break;
        }
    }
    if (i == args->len) {
        return str_printf("missing %s", name);
    }
    if (i + 1 >= args->len) {
        return str_printf("%s needs a value", name);
    }
    vec_remove(args, i);
    *value = (char *)args->items[i];
    vec_remove(args, i);
    return NULL;
}

static Vec arg_vec(int argc, char **argv)
{
    Vec v;
    int i;
    vec_init(&v);
    for (i = 0; i < argc; i++) {
        vec_push(&v, argv[i]);
    }
    return v;
}

/* `--stage` and `--package`, and the package directory. stages is a list
 * of names with NULL at the end. */
static char *stage_and_package(int argc, char **argv, const char *const *stages,
                               char **stage, char **package, char **pkg_dir)
{
    Vec args = arg_vec(argc, argv);
    char *e;
    size_t i;
    int known = 0;
    if ((e = take_option(&args, "--stage", stage)) != NULL) {
        return e;
    }
    if ((e = take_option(&args, "--package", package)) != NULL) {
        return e;
    }
    if (args.len > 0) {
        return str_printf("unexpected argument %s", (const char *)args.items[0]);
    }
    for (i = 0; stages[i] != NULL; i++) {
        if (str_eq(stages[i], *stage)) {
            known = 1;
        }
    }
    if (!known) {
        return str_printf("unknown stage %s", *stage);
    }
    *pkg_dir = package_dir(*package);
    if (!sys_is_dir(*pkg_dir)) {
        return str_printf("%s is not a directory", *pkg_dir);
    }
    return NULL;
}

/* --- Child processes ---------------------------------------------------- */

/* The first line of a text that is not blank, trimmed, or "". */
static char *first_line(const char *text)
{
    Vec lines = str_lines(text);
    size_t i;
    for (i = 0; i < lines.len; i++) {
        char *t = str_trim((const char *)lines.items[i]);
        if (t[0] != '\0') {
            return t;
        }
    }
    return xstrdup("");
}

/* A file name under BUILD_DIR for a temporary file of this process. */
static char *temp_path(const char *name)
{
    return str_printf("%s/%s-%lu", BUILD_DIR, name, sys_pid());
}

/* The contents of a file that a child wrote, as UTF-8 text with U+FFFD
 * for invalid bytes. "" when the file cannot be read. */
static char *read_output(const char *path)
{
    size_t n;
    char *data = read_file(path, &n);
    if (data == NULL) {
        return xstrdup("");
    }
    return utf8_lossy(data, n);
}

/* --- check -------------------------------------------------------------- */

typedef struct {
    int ok;
    char *error; /* optional */
    int has_pos;
    Pos pos;
} Report;

static Report report_fail(char *error, const Pos *pos)
{
    Report r;
    r.ok = 0;
    r.error = error;
    r.has_pos = pos != NULL;
    if (pos != NULL) {
        r.pos = *pos;
    } else {
        r.pos.line = 0;
        r.pos.col = 0;
    }
    return r;
}

static Report report_ok(void)
{
    Report r;
    r.ok = 1;
    r.error = NULL;
    r.has_pos = 0;
    r.pos.line = 0;
    r.pos.col = 0;
    return r;
}

/* The reference parser, `aihc-parse` from `tools/aihc-parse`, and the
 * language flags of one package. */
typedef struct {
    char *command; /* optional */
    /* `-X` flags from the package's `.cabal` file (char *). */
    Vec flags;
} Reference;

static Reference reference_for_package(const char *pkg_dir)
{
    Reference r;
    const char *env = getenv("AIHC_PARSE");
    CabalPackage *cabal;
    r.command = env != NULL ? xstrdup(env) : sys_find_in_path("aihc-parse");
    cabal = cabal_read(pkg_dir);
    if (cabal != NULL) {
        r.flags = cabal_ghc_flags(cabal);
    } else {
        vec_init(&r.flags);
    }
    return r;
}

/* The reference resolver, `resolve-oracle` from `tools/resolve-oracle`,
 * run once for each package on first use. */
typedef struct {
    const char *package;
    char *command; /* optional */
    int done;
    Dump dump;
    char *error; /* the error of the run, or NULL */
} Oracle;

static Oracle oracle_new(const char *package)
{
    Oracle o;
    const char *env = getenv("AIHC_RESOLVE_ORACLE");
    o.package = package;
    o.command = env != NULL ? xstrdup(env) : sys_find_in_path("resolve-oracle");
    o.done = 0;
    dump_init(&o.dump);
    o.error = NULL;
    return o;
}

/* Run the oracle on the package. The manifest goes to its standard input
 * through a temporary file. Returns NULL and fills *dump, or returns an
 * error message. */
static char *oracle_run(const Oracle *o, Dump *dump)
{
    char *text;
    char *e;
    char *in_path;
    char *out_path;
    char *err_path;
    char *argv[2];
    int status;
    int err;
    if (o->command == NULL) {
        return xstrdup("resolve-oracle not found: set AIHC_RESOLVE_ORACLE or add it "
                       "to PATH");
    }
    if ((e = manifest(o->package, &text)) != NULL) {
        return e;
    }
    in_path = temp_path("oracle.in");
    out_path = temp_path("oracle.out");
    err_path = temp_path("oracle.err");
    errno = 0;
    if ((err = sys_create_dir_all(BUILD_DIR)) != 0
        || write_file(in_path, text, strlen(text)) != 0) {
        if (err == 0) {
            err = errno != 0 ? errno : EIO;
        }
        return str_printf("cannot write to %s: %s", o->command, os_error(err));
    }
    argv[0] = o->command;
    argv[1] = NULL;
    err = sys_run(argv, in_path, out_path, err_path, &status);
    remove(in_path);
    if (err != 0) {
        remove(out_path);
        remove(err_path);
        return str_printf("cannot run %s: %s", o->command, os_error(err));
    }
    if (status != 0) {
        char *first = first_line(read_output(err_path));
        remove(out_path);
        remove(err_path);
        return str_printf("%s failed: %s", o->command, first);
    }
    text = read_output(out_path);
    remove(out_path);
    remove(err_path);
    if ((e = dump_parse(text, dump)) != NULL) {
        return str_printf("cannot read the output of %s: %s", o->command, e);
    }
    return NULL;
}

/* The oracle's records for the package. Returns NULL and sets *err when
 * the oracle fails. */
static const Dump *oracle_dump(Oracle *o, char **err)
{
    if (!o->done) {
        o->error = oracle_run(o, &o->dump);
        o->done = 1;
    }
    if (o->error != NULL) {
        *err = o->error;
        return NULL;
    }
    return &o->dump;
}

/* Read and parse a module for check and dump. Returns NULL and fills
 * *report on failure. */
static Module *read_module(const char *file, Report *report)
{
    size_t len;
    char *err;
    char *src = source_text(file, &len, &err);
    char *ext;
    SyntaxError serr;
    Module *module;
    if (src == NULL) {
        *report = report_fail(str_printf("cannot read file: %s", err), NULL);
        return NULL;
    }
    ext = path_extension(file);
    if (ext != NULL && str_eq(ext, "lhs")) {
        *report = report_fail(xstrdup("literate Haskell is not supported"), NULL);
        return NULL;
    }
    module = parse_source(src, len, &serr);
    if (module == NULL) {
        *report = report_fail(str_printf("%s: %s", serr.stage, serr.message), &serr.pos);
        return NULL;
    }
    return module;
}

/* program is NULL for the parse stage. program_err is the error of the
 * program, or NULL. */
static Report check_module(const char *file, const char *stage,
                           const Reference *reference, Oracle *oracle,
                           const Program *program, const char *program_err, const Tc *tc)
{
    Report report;
    Module *module = read_module(file, &report);
    char *printed;
    char *printed_path;
    char *dir;
    char **argv;
    char *err_path;
    size_t i;
    size_t n;
    int status;
    int err;
    OccVec ours;
    ResolveError *rerr;
    const Dump *dump;
    char *derr;
    const ModuleRecords *theirs;
    Disagreement d;
    if (module == NULL) {
        return report;
    }
    /* The round trip: aihc-parser must read the printed module as the
     * original. */
    if (reference->command == NULL) {
        return report_fail(xstrdup("aihc-parse not found: set AIHC_PARSE or add it to "
                                   "PATH"),
                           NULL);
    }
    printed = print_module(module);
    printed_path = path_join(BUILD_DIR "/roundtrip", file);
    dir = path_parent(printed_path);
    if (dir != NULL && (err = sys_create_dir_all(dir)) != 0) {
        return report_fail(str_printf("cannot create %s: %s", dir, os_error(err)), NULL);
    }
    errno = 0;
    if (write_file(printed_path, printed, strlen(printed)) != 0) {
        return report_fail(str_printf("cannot write %s: %s", printed_path,
                                      os_error(errno != 0 ? errno : EIO)),
                           NULL);
    }
    argv = (char **)xmalloc((reference->flags.len + 4) * sizeof *argv);
    n = 0;
    argv[n++] = reference->command;
    for (i = 0; i < reference->flags.len; i++) {
        argv[n++] = (char *)reference->flags.items[i];
    }
    argv[n++] = (char *)source_path(file);
    argv[n++] = printed_path;
    argv[n] = NULL;
    err_path = temp_path("roundtrip.err");
    err = sys_run(argv, NULL, NULL, err_path, &status);
    free(argv);
    if (err != 0) {
        remove(err_path);
        return report_fail(str_printf("cannot run %s: %s", reference->command,
                                      os_error(err)),
                           NULL);
    }
    if (status != 0) {
        char *first = first_line(read_output(err_path));
        remove(err_path);
        return report_fail(str_printf("roundtrip: %s", first), NULL);
    }
    remove(err_path);
    if (str_eq(stage, "parse")) {
        return report_ok();
    }
    /* Resolve: our records must agree with the oracle's. */
    if (program_err != NULL) {
        return report_fail(str_printf("resolve: %s", program_err), NULL);
    }
    if (program == NULL) {
        return report_fail(xstrdup("resolve: no program"), NULL);
    }
    rerr = resolve_module(program, file, &ours);
    if (rerr != NULL) {
        return report_fail(rerr->message, rerr->has_pos ? &rerr->pos : NULL);
    }
    dump = oracle_dump(oracle, &derr);
    if (dump == NULL) {
        return report_fail(str_printf("oracle: %s", derr), NULL);
    }
    theirs = (const ModuleRecords *)strmap_get(&dump->modules, source_path(file));
    if (theirs == NULL) {
        return report_fail(xstrdup("oracle: no record for this module"), NULL);
    }
    if (theirs->error != NULL) {
        return report_fail(str_printf("oracle: %s", theirs->error), NULL);
    }
    if (resolve_compare(&ours, &theirs->occs, &d)) {
        Pos pos;
        pos.line = d.span.start_line;
        pos.col = d.span.start_col;
        return report_fail(str_printf("resolve: %s: %s", recspan_str(d.span), d.message),
                           &pos);
    }
    if (str_eq(stage, "typecheck")) {
        Pos pos;
        const char *terr;
        if (tc == NULL) {
            return report_fail(xstrdup("typecheck: no typechecker"), NULL);
        }
        terr = tc_module_error(tc, module->name, &pos);
        if (terr != NULL) {
            return report_fail(xstrdup(terr), pos.line != 0 ? &pos : NULL);
        }
    }
    return report_ok();
}

/* A JSON string literal for s. */
static void json_string(StrBuf *out, const char *s)
{
    static const char hex[] = "0123456789abcdef";
    sb_putc(out, '"');
    while (*s != '\0') {
        unsigned long c = utf8_next(&s);
        switch (c) {
        case '"':
            sb_puts(out, "\\\"");
            break;
        case '\\':
            sb_puts(out, "\\\\");
            break;
        case '\n':
            sb_puts(out, "\\n");
            break;
        case '\r':
            sb_puts(out, "\\r");
            break;
        case '\t':
            sb_puts(out, "\\t");
            break;
        default:
            if (c < 0x20 || (c >= 0x7F && c <= 0x9F)) {
                sb_puts(out, "\\u00");
                sb_putc(out, hex[(c >> 4) & 0xF]);
                sb_putc(out, hex[c & 0xF]);
            } else {
                sb_put_utf8(out, c);
            }
            break;
        }
    }
    sb_putc(out, '"');
}

static char *check(int argc, char **argv)
{
    static const char *const stages[] = {"parse", "resolve", "typecheck", NULL};
    char *stage;
    char *package;
    char *pkg_dir;
    char *e = stage_and_package(argc, argv, stages, &stage, &package, &pkg_dir);
    Reference reference;
    Oracle oracle;
    Program *prog = NULL;
    char *program_err = NULL;
    Vec files;
    Vec packages;
    StrBuf out;
    size_t i;
    Tc *tc = NULL;
    if (e != NULL) {
        return e;
    }
    reference = reference_for_package(pkg_dir);
    oracle = oracle_new(package);
    /* The resolver needs the top-level names of the package and of its
     * dependencies. */
    if (!str_eq(stage, "parse")) {
        program_err = program(package, &prog, &packages);
    }
    if (str_eq(stage, "typecheck") && program_err == NULL) {
        tc = tc_new(prog);
        for (i = 0; i < packages.len; i++) {
            tc_package(tc, (const Package *)packages.items[i]);
        }
    }
    files = haskell_files(pkg_dir);
    sb_init(&out);
    for (i = 0; i < files.len; i++) {
        const char *file = (const char *)files.items[i];
        Report report = check_module(file, stage, &reference, &oracle, prog, program_err, tc);
        sb_clear(&out);
        sb_puts(&out, "{\"file\": ");
        json_string(&out, file);
        sb_printf(&out, ", \"ok\": %s", report.ok ? "true" : "false");
        if (report.error != NULL) {
            sb_puts(&out, ", \"error\": ");
            json_string(&out, report.error);
        }
        if (report.has_pos) {
            sb_printf(&out, ", \"line\": %lu, \"col\": %lu", report.pos.line,
                      report.pos.col);
        }
        sb_puts(&out, "}\n");
        if ((e = out_write(out.s, out.len)) != NULL) {
            return e;
        }
    }
    return NULL;
}

/* --- manifest, dump and oracle ------------------------------------------ */

static char *print_manifest(int argc, char **argv)
{
    Vec args = arg_vec(argc, argv);
    char *package;
    char *text;
    char *e;
    if ((e = take_option(&args, "--package", &package)) != NULL) {
        return e;
    }
    if (args.len > 0) {
        return str_printf("unexpected argument %s", (const char *)args.items[0]);
    }
    if ((e = manifest(package, &text)) != NULL) {
        return e;
    }
    return out_str(text);
}

static char *dump(int argc, char **argv)
{
    static const char *const stages[] = {"resolve", NULL};
    char *stage;
    char *package;
    char *pkg_dir;
    Program *prog;
    Dump d;
    Vec files;
    StrBuf text;
    size_t i;
    char *e = stage_and_package(argc, argv, stages, &stage, &package, &pkg_dir);
    if (e != NULL) {
        return e;
    }
    if ((e = program(package, &prog, NULL)) != NULL) {
        return e;
    }
    dump_init(&d);
    files = haskell_files(pkg_dir);
    for (i = 0; i < files.len; i++) {
        const char *file = (const char *)files.items[i];
        Report report;
        ModuleRecords *records = (ModuleRecords *)xmalloc(sizeof *records);
        records->error = NULL;
        occvec_init(&records->occs);
        if (read_module(file, &report) == NULL) {
            records->error = report.error;
        } else {
            ResolveError *rerr = resolve_module(prog, file, &records->occs);
            if (rerr != NULL) {
                records->error = rerr->message;
                occvec_init(&records->occs);
            }
        }
        strmap_put(&d.modules, file, records);
    }
    sb_init(&text);
    dump_write(&d, &text);
    return out_write(text.s, text.len);
}

static char *oracle(int argc, char **argv)
{
    static const char *const stages[] = {"resolve", NULL};
    char *stage;
    char *package;
    char *pkg_dir;
    Oracle o;
    const Dump *d;
    StrBuf text;
    char *err;
    char *e = stage_and_package(argc, argv, stages, &stage, &package, &pkg_dir);
    if (e != NULL) {
        return e;
    }
    o = oracle_new(package);
    d = oracle_dump(&o, &err);
    if (d == NULL) {
        return str_printf("oracle: %s", err);
    }
    sb_init(&text);
    dump_write(d, &text);
    return out_write(text.s, text.len);
}

/* --- lex, parse and print ----------------------------------------------- */

/* Read the one file of lex, parse or print. Returns NULL and sets *err on
 * failure. */
static char *read_one(int argc, char **argv, const char *command, size_t *len,
                      char **err)
{
    char *src;
    char *e;
    if (argc != 1) {
        *err = str_printf("%s takes one file", command);
        return NULL;
    }
    src = source_text(argv[0], len, &e);
    if (src == NULL) {
        *err = str_printf("%s: %s", argv[0], e);
    }
    return src;
}

static char *syntax_error(const char *file, const SyntaxError *e)
{
    return str_printf("%s:%lu:%lu: %s: %s", file, e->pos.line, e->pos.col, e->stage,
                      e->message);
}

static char *parse(int argc, char **argv)
{
    size_t len;
    char *err;
    char *src = read_one(argc, argv, "parse", &len, &err);
    SyntaxError serr;
    Module *module;
    char *e;
    if (src == NULL) {
        return err;
    }
    module = parse_source(src, len, &serr);
    if (module == NULL) {
        return syntax_error(argv[0], &serr);
    }
    if ((e = out_str(debug_module(module))) != NULL) {
        return e;
    }
    return out_str("\n");
}

static char *print(int argc, char **argv)
{
    size_t len;
    char *err;
    char *src = read_one(argc, argv, "print", &len, &err);
    SyntaxError serr;
    Module *module;
    if (src == NULL) {
        return err;
    }
    module = parse_source(src, len, &serr);
    if (module == NULL) {
        return syntax_error(argv[0], &serr);
    }
    return out_str(print_module(module));
}

static char *lex_file(int argc, char **argv)
{
    size_t len;
    char *err;
    char *src = read_one(argc, argv, "lex", &len, &err);
    SyntaxError serr;
    TokenVec tokens;
    StrBuf out;
    size_t i;
    if (src == NULL) {
        return err;
    }
    memset(&tokens, 0, sizeof tokens);
    if (tokenize(src, len, &tokens, &serr) != 0) {
        return syntax_error(argv[0], &serr);
    }
    sb_init(&out);
    for (i = 0; i < tokens.len; i++) {
        const Token *tok = &tokens.items[i];
        sb_printf(&out, "%lu:%lu\t%s\n", tok->pos.line, tok->pos.col, token_str(tok));
    }
    return out_write(out.s, out.len);
}

/* --- run and core ------------------------------------------------------- */

/* The package that a program runs against: PKG for a file under
 * tests/eval/PKG/ or vendor/PKG/ when PKG is vendored, and the boot
 * `base` otherwise. A file under vendor/PKG/ is the main module of an
 * executable of PKG, for example vendor/aihc/exe/Main.hs. */
static const char *eval_package(const char *file, const StrMap *vendored)
{
    const char *rest;
    const char *slash;
    char *name;
    if (str_starts_with(file, "tests/eval/")) {
        rest = file + strlen("tests/eval/");
    } else if (str_starts_with(file, "vendor/")) {
        rest = file + strlen("vendor/");
    } else {
        return "base";
    }
    slash = strchr(rest, '/');
    if (slash == NULL) {
        return "base";
    }
    name = xstrndup(rest, (size_t)(slash - rest));
    return strmap_has(vendored, name) ? name : "base";
}

/* The program for a single-module file: the package it runs against
 * (see eval_package) and its dependencies as the resolver loads them,
 * plus the file as the module of a package named "main". Fills
 * *packages with the Package * list the desugarer takes. */
static char *program_for_file(const char *file, Program **out, Vec *packages)
{
    StrMap vendored = vendored_packages();
    Vec order;
    Vec builtins;
    Program *p;
    size_t i;
    size_t j;
    Package *main_pkg;
    SourceModule *source;
    Report report;
    Module *module;
    const char *target = eval_package(file, &vendored);
    const CabalPackage *target_cabal = NULL;
    char *e = package_order(target, &vendored, &order);
    if (e != NULL) {
        return e;
    }
    if (str_starts_with(file, "vendor/")) {
        target_cabal = (const CabalPackage *)strmap_get(&vendored, target);
    }
    builtins = builtin_modules(&order);
    p = program_new(&builtins);
    vec_init(packages);
    for (i = 0; i < order.len; i++) {
        const char *name = (const char *)order.items[i];
        const CabalPackage *cabal = (const CabalPackage *)strmap_get(&vendored, name);
        Vec files = haskell_files(package_dir(name));
        Package *package = (Package *)xcalloc(1, sizeof *package);
        package->name = xstrdup(name);
        package->extensions = cabal->extensions;
        vec_init(&package->modules);
        for (j = 0; j < files.len; j++) {
            const char *path = (const char *)files.items[j];
            Module *m;
            /* The main module goes into the package "main" below. */
            if (path_cmp(path, file) == 0) {
                continue;
            }
            m = read_module(path, &report);
            if (m == NULL) {
                return str_printf("%s: %s", path, report.error);
            }
            source = (SourceModule *)xmalloc(sizeof *source);
            source->path = xstrdup(path);
            source->module = m;
            vec_push(&package->modules, source);
        }
        program_add_package(p, package);
        vec_push(packages, package);
    }
    module = read_module(file, &report);
    if (module == NULL) {
        return str_printf("%s: %s", file, report.error);
    }
    main_pkg = (Package *)xcalloc(1, sizeof *main_pkg);
    main_pkg->name = xstrdup("main");
    if (target_cabal != NULL) {
        main_pkg->extensions = target_cabal->extensions;
    } else {
        vec_init(&main_pkg->extensions);
    }
    vec_init(&main_pkg->modules);
    source = (SourceModule *)xmalloc(sizeof *source);
    source->path = xstrdup(file);
    source->module = module;
    vec_push(&main_pkg->modules, source);
    program_add_package(p, main_pkg);
    vec_push(packages, main_pkg);
    *out = p;
    return NULL;
}

static char *desugar_file(int argc, char **argv, const char *command, CoreProgram *core)
{
    Program *p = NULL;
    Vec packages;
    char *e;
    Tc *tc;
    size_t i;
    if (argc != 1) {
        return str_printf("%s takes one file", command);
    }
    if ((e = program_for_file(argv[0], &p, &packages)) != NULL) {
        return e;
    }
    tc = tc_new(p);
    for (i = 0; i < packages.len; i++) {
        tc_package(tc, (const Package *)packages.items[i]);
    }
    return desugar_program(p, tc, &packages, core);
}

static char *run(int argc, char **argv)
{
    CoreProgram core;
    char *e = desugar_file(argc, argv, "run", &core);
    if (e != NULL) {
        return e;
    }
    return eval_run(&core);
}

static char *print_core(int argc, char **argv)
{
    CoreProgram core;
    StrBuf out;
    size_t i;
    int all = 0;
    char *e;
    /* `--all` prints the globals of every module, for debugging. */
    if (argc == 2 && str_eq(argv[0], "--all")) {
        all = 1;
        argc--;
        argv++;
    }
    e = desugar_file(argc, argv, "core", &core);
    if (e != NULL) {
        return e;
    }
    sb_init(&out);
    for (i = 0; i < core.globals.len; i++) {
        const Global *g = (const Global *)core.globals.items[i];
        if (all || str_eq(g->module, "Main")) {
            core_print_global(&out, g);
        }
    }
    return out_write(out.s, out.len);
}

/* --- main ---------------------------------------------------------------- */

int main(int argc, char **argv)
{
    const char *command = argc > 1 ? argv[1] : "";
    int rest_argc = argc > 1 ? argc - 2 : 0;
    char **rest = argv + (argc > 1 ? 2 : argc);
    char *result;
    sys_ignore_sigpipe();
    if (argc > 1 && str_eq(command, "check")) {
        result = check(rest_argc, rest);
    } else if (argc > 1 && str_eq(command, "manifest")) {
        result = print_manifest(rest_argc, rest);
    } else if (argc > 1 && str_eq(command, "dump")) {
        result = dump(rest_argc, rest);
    } else if (argc > 1 && str_eq(command, "oracle")) {
        result = oracle(rest_argc, rest);
    } else if (argc > 1 && str_eq(command, "lex")) {
        result = lex_file(rest_argc, rest);
    } else if (argc > 1 && str_eq(command, "parse")) {
        result = parse(rest_argc, rest);
    } else if (argc > 1 && str_eq(command, "print")) {
        result = print(rest_argc, rest);
    } else if (argc > 1 && str_eq(command, "run")) {
        result = run(rest_argc, rest);
    } else if (argc > 1 && str_eq(command, "core")) {
        result = print_core(rest_argc, rest);
    } else if (argc > 1 && (str_eq(command, "--help") || str_eq(command, "-h"))) {
        result = out_str(USAGE);
    } else {
        result = xstrndup(USAGE, strlen(USAGE) - 1);
    }
    if (result == NULL) {
        return 0;
    }
    /* A closed pipe (for example `aihc-boot lex FILE | head`) is not an
     * error worth a message. */
    if (strstr(result, "Broken pipe") != NULL) {
        return 0;
    }
    fprintf(stderr, "aihc-boot: %s\n", result);
    return 1;
}
