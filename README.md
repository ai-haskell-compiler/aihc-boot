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
_Last change in progress: 2026-09-25. Boot compiler: built._

| Milestone | Done | Progress |
| --- | ---: | --- |
| **M1** Vendor | 11/19 | `██████░░░░` 57.9% |
| **M2** Prune | 1/20 | `░░░░░░░░░░` 5.0% |
| **M3** Parse | 232/238 | `██████████` 97.5% |
| **M4** Resolve | 6/238 | `░░░░░░░░░░` 2.5% |
| **M5** Typecheck | 0/238 | `░░░░░░░░░░` 0.0% |
| **M6** Eval | — | `░░░░░░░░░░` 0.0% |
| **M7** Stage 1 | — | `░░░░░░░░░░` 0.0% |

<details><summary>Per-package breakdown</summary>

| Package | Plan | Status | Modules | Lines (upstream → now) | Parse | Resolve | Typecheck | Eval |
| --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| aihc | keep | vendored | 94 | 46705 → 46705 | 94/94 | 0/94 | 0/94 | — |
| aihc-resolve | keep | vendored | 8 | 4106 → 4106 | 8/8 | 0/8 | 0/8 | — |
| aihc-tc | keep | vendored | 54 | 22234 → 22234 | 54/54 | 0/54 | 0/54 | — |
| aihc-package-plan | keep | vendored | 5 | 2025 → 2025 | 5/5 | 0/5 | 0/5 | — |
| aihc-hackage | simplify | not vendored |  |  |  |  |  | — |
| aihc-parser | keep | vendored | 28 | 17315 → 17315 | 28/28 | 0/28 | 0/28 | — |
| aihc-cpp | keep | vendored | 6 | 2376 → 2376 | 6/6 | 0/6 | 0/6 | — |
| base | replace | vendored | 5 | 94 | 5/5 | 5/5 | 0/5 | — |
| ghc-prim | replace | not vendored |  |  |  |  |  | — |
| containers | simplify | vendored | 6 | 22364 → 614 | 6/6 | 0/6 | 0/6 | — |
| text | simplify | vendored | 14 | 20301 → 1026 | 13/14 | 0/14 | 0/14 | — |
| bytestring | simplify | vendored | 15 | 15180 → 2570 | 10/15 | 0/15 | 0/15 | — |
| deepseq | simplify | vendored | 1 | 936 → 72 | 1/1 | 1/1 | 0/1 | — |
| transformers | simplify | not vendored |  |  |  |  |  | — |
| array | simplify | not vendored |  |  |  |  |  | — |
| binary | simplify | not vendored |  |  |  |  |  | — |
| filepath | simplify | vendored | 2 | 3156 → 214 | 2/2 | 0/2 | 0/2 | — |
| directory | replace | not vendored |  |  |  |  |  | — |
| process | replace | not vendored |  |  |  |  |  | — |
| unix | drop | still used | | | | | | |
| time | drop | still used | | | | | | |
| stm | drop | still used | | | | | | |
| async | drop | still used | | | | | | |
| megaparsec | simplify | not vendored |  |  |  |  |  | — |
| prettyprinter | simplify | not vendored |  |  |  |  |  | — |
| Cabal-syntax | replace | not vendored |  |  |  |  |  | — |
| Cabal | drop | still used | | | | | | |
| aeson | replace | not vendored |  |  |  |  |  | — |
| optparse-applicative | replace | not vendored |  |  |  |  |  | — |
| cryptohash-sha256 | replace | not vendored |  |  |  |  |  | — |
| vector | simplify | not vendored |  |  |  |  |  | — |
| primitive | simplify | not vendored |  |  |  |  |  | — |
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
nix develop                          # Python, GHC 9.12, cabal, cargo and rustc
cargo build --release                # build target/release/aihc-boot
cargo test                           # run the unit tests
./target/release/aihc-boot check --stage parse --package aihc-cpp    # needs aihc-parse
./target/release/aihc-boot check --stage resolve --package aihc-cpp  # needs resolve-oracle too
./target/release/aihc-boot oracle --stage resolve --package aihc-cpp # what aihc-resolve says
./target/release/aihc-boot parse vendor/aihc-cpp/src/Aihc/Cpp/Cursor.hs
./target/release/aihc-boot print vendor/aihc-cpp/src/Aihc/Cpp/Cursor.hs
python3 scripts/progress.py          # print the progress report
python3 scripts/progress.py --write  # update this README and the history
python3 scripts/vendor.py deepseq    # vendor one package from boot.toml
python3 scripts/vendor.py --all      # vendor everything still missing
```
