/* The C preprocessor step. A module with the CPP extension goes through
 * clang, as GHC sends it through cc: `clang -E -undef -traditional`.
 * The result goes to build/cpp/PATH, and the oracles read that file, so
 * both sides of a comparison see the same source.
 */
#ifndef AIHC_CPP_H
#define AIHC_CPP_H

#include <stddef.h>

/* The source text of a module as the lexer sees it: the preprocessed
 * text when the module uses CPP, and the file itself otherwise. Returns
 * NULL and sets *err on failure. Sets *len when len is not NULL. */
char *source_text(const char *path, size_t *len, char **err);

/* The file that holds the source text of a module: build/cpp/PATH when
 * the module uses CPP, and PATH itself otherwise. */
const char *source_path(const char *path);

#endif
