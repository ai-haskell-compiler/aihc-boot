#!/bin/sh
# Run the tests of aihc-boot. Run it from the repository root, after
# `make`: `make check` does both.
#
# 1. Golden tests: tests/syntax/NAME.hs is the input. NAME.lex, NAME.parse
#    and NAME.print hold the expected output (stdout and stderr) of
#    `aihc-boot lex`, `parse` and `print`.
# 2. Round trip: every vendored module that parses must survive a print
#    and a second parse with the same printed text.
#
# 3. Eval tests: `aihc-boot run tests/eval/PKG/NAME.hs` must print
#    NAME.stdout and exit with 0. NAME.stdin, when present, is its
#    standard input. Each test runs twice: once with the default heap,
#    and once with a very small heap (AIHC_BOOT_HEAP), so the collector
#    runs often.
# 4. Core golden tests: tests/core/NAME.hs with NAME.core, the output of
#    `aihc-boot core`.
#
# To accept a new expected output, run `sh tests/run.sh --update`.

AIHC_BOOT=${AIHC_BOOT:-build/aihc-boot}
TMP=build/test
mkdir -p "$TMP"
update=0
[ "$1" = "--update" ] && update=1
failed=0
passed=0

golden() {
    # golden COMMAND INPUT EXPECTED
    "$AIHC_BOOT" "$1" "$2" > "$TMP/out" 2>&1
    if [ $update = 1 ]; then
        cp "$TMP/out" "$3"
    fi
    if cmp -s "$TMP/out" "$3"; then
        passed=$((passed + 1))
    else
        echo "FAIL: aihc-boot $1 $2"
        diff "$3" "$TMP/out" | head -20
        failed=$((failed + 1))
    fi
}

for input in tests/syntax/*.hs; do
    base=${input%.hs}
    for command in lex parse print; do
        if [ -f "$base.$command" ]; then
            golden "$command" "$input" "$base.$command"
        fi
    done
done

for input in tests/core/*.hs; do
    base=${input%.hs}
    if [ -f "$base.core" ]; then
        golden core "$input" "$base.core"
    fi
done

for input in tests/eval/*/*.hs; do
    base=${input%.hs}
    expected="$base.stdout"
    stdin=/dev/null
    [ -f "$expected" ] || continue
    [ -f "$base.stdin" ] && stdin="$base.stdin"
    for heap in 0 1024; do
        if AIHC_BOOT_HEAP=$heap "$AIHC_BOOT" run "$input" < "$stdin" > "$TMP/out" 2> "$TMP/err" \
            && cmp -s "$TMP/out" "$expected"; then
            passed=$((passed + 1))
        else
            echo "FAIL: aihc-boot run $input (AIHC_BOOT_HEAP=$heap)"
            diff "$expected" "$TMP/out" | head -20
            head -5 "$TMP/err"
            failed=$((failed + 1))
        fi
    done
done

roundtrip=0
for file in $(find vendor -name '*.hs' -o -name '*.hs-boot' | sort); do
    "$AIHC_BOOT" print "$file" > "$TMP/first.hs" 2> /dev/null || continue
    if ! "$AIHC_BOOT" print "$TMP/first.hs" > "$TMP/second.hs" 2> "$TMP/err"; then
        echo "FAIL: $file: the printed source does not parse: $(cat "$TMP/err")"
        failed=$((failed + 1))
    elif ! cmp -s "$TMP/first.hs" "$TMP/second.hs"; then
        echo "FAIL: $file: the second print differs"
        failed=$((failed + 1))
    else
        roundtrip=$((roundtrip + 1))
    fi
done
passed=$((passed + roundtrip))
echo "$roundtrip vendored modules survive a round trip"

echo "$passed passed, $failed failed"
[ $failed = 0 ]
