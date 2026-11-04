/* The vendored packages: what `vendor/NAME/NAME.cabal` says about a
 * package, and the manifest that `tools/resolve-oracle` reads. This file
 * also has the path and file helpers that main.c uses.
 */
#ifndef AIHC_MANIFEST_H
#define AIHC_MANIFEST_H

#include "resolve.h"
#include "strmap.h"
#include "util.h"

/* --- Paths and files ---------------------------------------------------- */

/* a/b, as Rust's Path::join: b replaces a when b is absolute, and no
 * second `/` comes between them. */
char *path_join(const char *a, const char *b);
/* Compare two paths component by component, as Rust's Path ordering. */
int path_cmp(const char *a, const char *b);
/* The extension of the last component of a path, as Rust's
 * Path::extension, or NULL. */
char *path_extension(const char *path);
/* The directory part of a path, or NULL when the path has none. */
char *path_parent(const char *path);

/* An OS error in the form of Rust's io::Error: "MESSAGE (os error N)". */
char *os_error(int err);
/* Read a whole UTF-8 text file, as Rust's fs::read_to_string. Returns
 * NULL and sets *err to the error message on failure. Sets *len when len
 * is not NULL. */
char *read_text(const char *path, size_t *len, char **err);
/* Bytes as UTF-8, with U+FFFD for invalid bytes, as Rust's
 * String::from_utf8_lossy. */
char *utf8_lossy(const char *s, size_t n);

/* --- .cabal files ------------------------------------------------------- */

/* The fields of a package's `.cabal` file that the boot compiler reads. */
typedef struct {
    /* The first `default-language` value, or NULL. */
    char *language;
    /* Every `default-extensions` value (char *), in order, without
     * duplicates. */
    Vec extensions;
    /* Every package name in a `build-depends` field, as the keys of a
     * set (the values are not used). */
    StrMap depends;
    /* The `version` field, or NULL. */
    char *version;
    /* The `cpp-options` and the `include-dirs` of the main library and
     * of the common stanzas that it imports (char *). The reader
     * evaluates `if` conditions for GHC 9.12 on a Unix-like system, with
     * the default flags, as `scripts/progress.py` does. */
    Vec cpp_options;
    Vec include_dirs;
} CabalPackage;

/* Read the first `.cabal` file in a package directory. NULL when there is
 * none, or when it cannot be read. */
CabalPackage *cabal_read(const char *pkg_dir);
/* Read the fields from the text of a `.cabal` file. */
CabalPackage *cabal_parse(const char *text);
/* The `-X` flags for GHC (char *): the language first, then the
 * extensions. */
Vec cabal_ghc_flags(const CabalPackage *p);

/* --- Packages ----------------------------------------------------------- */

/* Every Haskell source in a package (char *), sorted, with the same rules
 * as `scripts/progress.py`: any `.hs`, `.lhs`, `.hsc` or `.hs-boot` file,
 * except `Setup.hs` at the package root. */
Vec haskell_files(const char *pkg_dir);
/* The directory of a vendored package. */
char *package_dir(const char *name);
/* Every vendored package: each directory under `vendor/` with a `.cabal`
 * file, by directory name (CabalPackage *). */
StrMap vendored_packages(void);
/* The target package and the vendored packages it depends on (char *), in
 * dependency order: a package comes after every package it depends on.
 * Returns NULL on success. Returns an error message when the target is
 * not vendored, or when the dependencies contain a cycle. */
char *package_order(const char *target, const StrMap *packages, Vec *out);
/* The manifest for `tools/resolve-oracle`: the target package and its
 * vendored dependencies, in dependency order, with their modules. Only
 * the target's records are reported. The manifest also names the builtin
 * modules of the packages (see builtin_modules). Returns NULL on success,
 * or an error message (see package_order). */
char *manifest(const char *target, char **out);
/* The target package and its vendored dependencies, parsed, for the
 * resolver. The program leaves out a module that does not parse: such a
 * module has no exports.
 * Returns NULL on success, or an error message (see package_order). */
char *program(const char *target, Program **out, Vec *packages_out);

#endif
