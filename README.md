# aihc-boot

[![Progress](https://github.com/ai-haskell-compiler/aihc-boot/actions/workflows/progress.yml/badge.svg)](https://github.com/ai-haskell-compiler/aihc-boot/actions/workflows/progress.yml)
[![CI](https://github.com/ai-haskell-compiler/aihc-boot/actions/workflows/ci.yml/badge.svg)](https://github.com/ai-haskell-compiler/aihc-boot/actions/workflows/ci.yml)

A minimal Haskell compiler whose only goal is to compile
[aihc](https://github.com/ai-haskell-compiler/aihc) and the vendored,
simplified dependencies it needs. It only has to support the Haskell those
packages use. See [docs/PLAN.md](docs/PLAN.md) for the milestones and how
they are measured.

## Progress

Updated daily by [a scheduled workflow](.github/workflows/progress.yml);
history in [progress/history.csv](progress/history.csv).

<!-- progress:start -->
_Last change in progress: 2026-11-05. Boot compiler: built._

| Milestone | Done | Progress |
| --- | ---: | --- |
| **M1** Vendor | 29/29 | `██████████` 100.0% |
| **M2** Prune | 1/19 | `█░░░░░░░░░` 5.3% |
| **M3** Parse | 527/528 | `██████████` 99.8% |
| **M4** Resolve | 136/528 | `███░░░░░░░` 25.8% |
| **M5** Typecheck | 128/528 | `██░░░░░░░░` 24.2% |
| **M6** Eval | 17/17 | `██████████` 100.0% |
| **M7** Stage 1 | — | `░░░░░░░░░░` 0.0% |

<details><summary>Per-package breakdown</summary>

| Package | Plan | Status | Modules | Lines (upstream → now) | Parse | Resolve | Typecheck | Eval |
| --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| aihc | keep | vendored | 94 | 46705 → 46705 | 94/94 | 2/94 | 1/94 | — |
| aihc-resolve | keep | vendored | 8 | 4106 → 4106 | 8/8 | 0/8 | 0/8 | — |
| aihc-tc | keep | vendored | 54 | 22234 → 22234 | 54/54 | 0/54 | 0/54 | — |
| aihc-package-plan | keep | vendored | 5 | 2025 → 2025 | 5/5 | 0/5 | 0/5 | — |
| aihc-hackage | simplify | vendored | 11 | 2226 → 2226 | 11/11 | 2/11 | 2/11 | — |
| aihc-parser | keep | vendored | 28 | 17315 → 17315 | 28/28 | 0/28 | 0/28 | — |
| aihc-cpp | keep | vendored | 6 | 2376 → 2376 | 6/6 | 1/6 | 1/6 | — |
| base | replace | vendored | 74 | 5854 | 74/74 | 74/74 | 74/74 | 15/15 |
| ghc-prim | replace | not vendored |  |  |  |  |  | — |
| containers | simplify | vendored | 6 | 22364 → 614 | 6/6 | 6/6 | 6/6 | — |
| text | simplify | vendored | 14 | 20301 → 1040 | 14/14 | 14/14 | 14/14 | 2/2 |
| bytestring | simplify | vendored | 15 | 15180 → 2548 | 15/15 | 15/15 | 15/15 | — |
| deepseq | simplify | vendored | 1 | 936 → 72 | 1/1 | 1/1 | 1/1 | — |
| transformers | simplify | vendored | 24 | 5319 → 5319 | 24/24 | 6/24 | 1/24 | — |
| array | simplify | vendored | 15 | 2620 → 2620 | 15/15 | 0/15 | 0/15 | — |
| binary | simplify | vendored | 9 | 2820 → 2820 | 9/9 | 1/9 | 1/9 | — |
| filepath | simplify | vendored | 2 | 3156 → 214 | 2/2 | 2/2 | 2/2 | — |
| directory | replace | not vendored |  |  |  |  |  | — |
| process | replace | not vendored |  |  |  |  |  | — |
| unix | drop | still used | | | | | | |
| time | drop | still used | | | | | | |
| stm | simplify | vendored | 10 | 1282 → 1282 | 10/10 | 0/10 | 0/10 | — |
| async | drop | still used | | | | | | |
| megaparsec | simplify | vendored | 19 | 5541 → 5541 | 19/19 | 0/19 | 0/19 | — |
| prettyprinter | simplify | vendored | 28 | 3662 → 3592 | 28/28 | 1/28 | 0/28 | — |
| mtl | simplify | vendored | 24 | 2607 → 2607 | 24/24 | 1/24 | 0/24 | — |
| parser-combinators | simplify | vendored | 7 | 1124 → 1124 | 7/7 | 5/7 | 5/7 | — |
| case-insensitive | simplify | vendored | 3 | 205 → 205 | 3/3 | 0/3 | 0/3 | — |
| scientific | simplify | vendored | 5 | 1227 → 1223 | 5/5 | 0/5 | 0/5 | — |
| hashable | simplify | vendored | 10 | 1541 → 1541 | 10/10 | 2/10 | 2/10 | — |
| integer-logarithms | simplify | vendored | 4 | 510 → 510 | 4/4 | 2/4 | 2/4 | — |
| os-string | simplify | vendored | 12 | 3820 → 3712 | 11/12 | 0/12 | 0/12 | — |
| exceptions | simplify | vendored | 2 | 957 → 954 | 2/2 | 0/2 | 0/2 | — |
| vector-stream | simplify | vendored | 1 | 1448 → 1448 | 1/1 | 0/1 | 0/1 | — |
| Cabal-syntax | replace | not vendored |  |  |  |  |  | — |
| Cabal | drop | still used | | | | | | |
| aeson | replace | not vendored |  |  |  |  |  | — |
| optparse-applicative | replace | not vendored |  |  |  |  |  | — |
| cryptohash-sha256 | replace | not vendored |  |  |  |  |  | — |
| vector | simplify | vendored | 23 | 19297 → 19297 | 23/23 | 1/23 | 1/23 | — |
| primitive | simplify | vendored | 14 | 5044 → 5000 | 14/14 | 0/14 | 0/14 | — |
| haskeline | drop | still used | | | | | | |
| libffi | drop | still used | | | | | | |
| http-client | drop | still used | | | | | | |
| http-client-tls | drop | still used | | | | | | |
| http-types | drop | still used | | | | | | |
| tar | drop | still used | | | | | | |
| zlib | drop | still used | | | | | | |

</details>
<!-- progress:end -->

| | Milestone |
| --- | --- |
| M1 | Vendor aihc and its dependencies into `vendor/` |
| M2 | Prune the closure: drop and replace dependencies |
| M3 | Parse every vendored module |
| M4 | Resolve names in every vendored module |
| M5 | Typecheck every vendored module |
| M6 | Eval tests pass for every vendored package |
| M7 | aihc-boot builds a working `aihc` (stage 1) |

## Development

```bash
nix develop                          # Python, GHC 9.12, cabal and a C compiler
make                                 # build build/aihc-boot from the C89 sources
make check                           # run the tests
./build/aihc-boot check --stage parse --package aihc-cpp    # needs aihc-parse
./build/aihc-boot check --stage resolve --package aihc-cpp  # needs resolve-oracle too
./build/aihc-boot oracle --stage resolve --package aihc-cpp # what aihc-resolve says
./build/aihc-boot parse vendor/aihc-cpp/src/Aihc/Cpp/Cursor.hs
./build/aihc-boot print vendor/aihc-cpp/src/Aihc/Cpp/Cursor.hs
python3 scripts/progress.py          # print the progress report
python3 scripts/progress.py --write  # update this README and the history
python3 scripts/vendor.py deepseq    # vendor one package from boot.toml
python3 scripts/vendor.py --all      # vendor everything still missing
```
