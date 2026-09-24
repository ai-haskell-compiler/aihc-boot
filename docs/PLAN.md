# aihc-boot plan

aihc-boot is a minimal Haskell compiler with one job: compile
[aihc](https://github.com/ai-haskell-compiler/aihc) and the vendored,
simplified dependencies aihc needs. It is not a general Haskell compiler.
Anything aihc does not use, aihc-boot does not have to support.

## Strategy

1. **Pin the target.** Vendor aihc and its whole dependency closure at fixed
   revisions (`boot.toml`, `vendor/lock.json`). The vendored tree defines the
   language aihc-boot has to understand.
2. **Shrink the target.** Cut every dependency down to what aihc actually
   uses, replace large packages with small in-tree ones, and patch aihc to
   drop dependencies it can live without. Each deleted line is one less
   line for aihc-boot to handle.
3. **Keep GHC as the reference.** The vendored tree must keep building and
   passing its tests with GHC (the nix dev shell provides GHC 9.12 and
   cabal). This is what makes step 2 safe: every simplification is checked
   by a real compiler before aihc-boot has to handle it.
4. **Grow aihc-boot phase by phase**, measured per package and per module
   over the vendored tree: parse → resolve → typecheck → run.

## Milestones

Each milestone has a number that `scripts/progress.py` computes without
human input. The README shows them all, updated daily by
`.github/workflows/progress.yml`.

| | Milestone | Measured as | Done when |
| --- | --- | --- | --- |
| M1 | **Vendor**: every `keep`/`simplify` package from `boot.toml` is in `vendor/` | Vendored packages / planned packages | All vendored and `cabal build` with GHC works on the vendored tree |
| M2 | **Prune**: the closure is as small as we plan to make it | `drop` packages no longer referenced + `replace` packages written + unaccounted dependencies triaged | Every `drop` gone, every `replace` written, no unaccounted dependencies |
| M3 | **Parse**: aihc-boot parses the vendored tree | Modules parsed / total modules | 100% |
| M4 | **Resolve**: names and imports resolve | Modules resolved / total modules | 100% |
| M5 | **Typecheck**: including type classes and the extensions the tree uses | Modules typechecked / total modules | 100% |
| M6 | **Eval**: generated code runs correctly | Eval tests passing / total eval tests | 100%, with tests covering every vendored package |
| M7 | **Stage 1**: aihc-boot builds a working `aihc` | Stage-1 checks passing / total | `aihc` built by aihc-boot passes the stage-1 checks |

Notes:

- **M1 and M2 overlap in time.** Simplifying a package can start as soon as
  it is vendored. M2's denominator grows as vendoring reveals transitive
  dependencies (they show up as "unaccounted" until added to `boot.toml`).
- **M3–M5 share a denominator** (all Haskell modules in `vendor/`), which
  changes as M1 adds code and M2 removes it, so their percentages can move
  in both directions early on. Line counts per package ("upstream → now")
  track the shrinking separately.
- **M6 is per package.** Eval tests live in `tests/eval/<package>/NAME.hs`
  with the expected output in `NAME.stdout` (and optional input in
  `NAME.stdin`). They exercise each vendored package through its public
  API, e.g. `tests/eval/containers/map-insert.hs`, so we can say
  "containers works" before aihc as a whole compiles. Write them with GHC
  first; GHC produces the golden `.stdout`.
- **M7 checks** are shell scripts in `tests/stage1/*.sh`, run with
  `AIHC_BOOT` set, passing on exit code 0. Planned checks, in order: aihc-boot
  links `aihc`; `aihc --version` runs; stage-1 `aihc` compiles and runs
  hello-world; stage-1 `aihc` passes a chosen subset of aihc's own test
  suite.
- A possible **M8 (fixpoint)**: stage-1 `aihc` builds stage-2 `aihc`, and
  the stage-2 output matches the stage-1 output. That checks aihc more than
  aihc-boot, so it is left out of the tracked milestones for now.

## Package plans

`boot.toml` lists every package with one of four plans:

| Plan | Meaning | Where the code lives |
| --- | --- | --- |
| `keep` | First-party aihc code, vendored as-is apart from mechanical edits | `vendor/<name>/` (fetched) |
| `simplify` | Upstream package, cut down to the API aihc uses | `vendor/<name>/` (fetched, then edited) |
| `replace` | Small in-tree package with the same name and modules, covering only what aihc uses | `vendor/<name>/` (written by hand) |
| `drop` | Patch the packages that use it so the dependency goes away | nowhere |

The current plans are a first pass from aihc's `build-depends` and imports
at the pinned revision. The larger choices:

- **base / ghc-prim → replace.** aihc-boot ships its own small `base`,
  grown only as the vendored tree needs it. This is the largest single
  piece of work outside the compiler itself.
- **Cabal-syntax → replace, Cabal → drop.** Cabal-syntax is very large and aihc only
  reads a handful of `.cabal` fields. Dropping `Cabal` also means replacing
  aihc's `Custom` `Setup.hs` (it generates the build identity module).
- **aeson → replace.** It is used only for aihc's own lock files and
  manifests; a small JSON type with a parser and printer is enough. This also
  avoids aeson's large closure (attoparsec, scientific, hashable,
  unordered-containers, Template Haskell).
- **Network download, tar and zlib → drop.** aihc-hackage's index
  download does not need to work in a boot build: point aihc at a local
  package directory instead.
- **stm, async → drop.** A boot-built aihc can build sequentially.
- **haskeline → drop** (in `build-depends` but never imported), **libffi →
  drop** (only the GRIN interpreter uses it).
- **text, bytestring, containers, megaparsec, prettyprinter →
  simplify.** These are used everywhere; vendor them and delete what aihc
  does not use.

Changes to aihc itself (dropping dependencies, replacing the Custom setup)
are made in `vendor/aihc*` and should also go upstream where they make sense,
so re-vendoring a newer aihc stays cheap.

## Compiler interface

The tracker talks to aihc-boot only through this command-line contract, so
it does not depend on how aihc-boot is implemented. The command comes from
`[compiler].command` in `boot.toml` (default `./target/release/aihc-boot`), or from
the `AIHC_BOOT` environment variable. While the command does not exist,
M3–M7 read 0.

- `aihc-boot check --stage {parse|resolve|typecheck} --package NAME`
  checks every module of `vendor/NAME/` up to the given stage and prints one
  JSON object per module on stdout:
  `{"file": "vendor/NAME/src/Data/Foo.hs", "ok": true}`.
  Extra fields (such as `"error"`) are ignored. Modules missing from the
  output count as failures, so a crash only loses the modules after it.
  Resolving and typechecking a package needs its dependencies, so aihc-boot
  reads `boot.toml`/`vendor/` itself to find them.
- `aihc-boot run FILE.hs` compiles and runs a single-module program
  against the vendored packages. Used by the eval tests.
- Stage-1 scripts invoke `$AIHC_BOOT` however they need to.

## Open decisions

1. **Implementation language of aihc-boot.** The repository's `.gitignore`
   is Rust's template, and the default compiler path assumes a Cargo build.
   Writing it in a non-Haskell language makes aihc-boot a real bootstrap path
   (no GHC needed); writing it in Haskell would allow reusing aihc's parser
   but keep the GHC dependency. The tracker and this plan work with either.
2. **Code generation target for M6/M7:** C (easiest to debug, portable), a
   bytecode interpreter (smallest, and slow may be fine for a one-off
   bootstrap build), or native code.
3. **How far to simplify `text`:** keep a real UTF-8 array implementation,
   or back `Text` with `String` and accept the slower stage-1 compiler.
4. **Whether `vector`/`primitive` become thin wrappers over `array`.**

## Tooling

- `nix develop` gives the full dev shell (Python for the scripts, GHC 9.12
  and cabal as the reference compiler). `nix develop .#tracker` is the
  small shell the scheduled workflow uses.
- `scripts/vendor.py NAME` / `--all` fetches upstream sources into
  `vendor/` and records the version and baseline size in `vendor/lock.json`.
- `scripts/progress.py` prints the report; `--write` updates the README and
  `progress/history.csv`. It records a new history row only when some number
  changed, so the daily run commits nothing on days without progress.
- `nix flake check` runs the tracker on a clean checkout.
- The progress workflow pushes to `main` with a deploy key, because a
  ruleset cannot bypass `GITHUB_TOKEN`. To set up the key:
  1. Run `ssh-keygen -t ed25519 -N "" -C progress -f progress_key`.
  2. Add `progress_key.pub` as a deploy key with write access (Settings →
     Deploy keys).
  3. Add the contents of `progress_key` as the repository secret
     `PROGRESS_DEPLOY_KEY` (Settings → Secrets and variables → Actions).
  4. Add "Deploy keys" to the bypass list of the ruleset on `main`.
  5. Delete the two local key files.
