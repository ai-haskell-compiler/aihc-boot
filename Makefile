# Build aihc-boot with any C89 compiler. Plain POSIX make is enough.
#
#   make          build build/aihc-boot
#   make check    run the tests
#   make clean    remove build/
#
# Without make: mkdir -p build && cc -o build/aihc-boot src/*.c

CC = cc
CFLAGS = -O2 -g
WARN = -std=c89 -pedantic -Wall -Wextra -Werror

SRCS = src/compare.c src/core.c src/debug.c src/desugar.c src/eval.c src/fixity.c \
       src/layout.c src/lexer.c src/main.c src/manifest.c src/parse.c \
       src/parse_decls.c src/parse_exprs.c src/parse_pats.c \
       src/parse_types.c src/print.c src/record.c src/resolve.c \
       src/scope.c src/strmap.c src/sys.c src/tokens.c \
       src/tc_decl.c src/tc_derive.c src/tc_expr.c \
       src/tc_solve.c src/tc_type.c src/unicode_tables.c src/util.c
HDRS = src/core.h src/desugar.h src/eval.h src/fixity.h src/manifest.h src/parse.h \
       src/resolve.h src/strmap.h src/syntax.h src/sys.h src/tc.h src/tc_int.h \
       src/unicode_tables.h src/util.h

build/aihc-boot: $(SRCS) $(HDRS)
	mkdir -p build
	$(CC) $(CFLAGS) $(WARN) -o build/aihc-boot $(SRCS)

check: build/aihc-boot
	sh tests/run.sh

clean:
	rm -rf build

.PHONY: check clean
