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
| M6 | **Eval**: the interpreter runs the vendored packages correctly | Eval tests passing / total eval tests | 100%, with tests covering every vendored package |
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
- **text, bytestring, containers, prettyprinter → simplify.** These are
  used everywhere; vendor them and delete what aihc does not use.
- **megaparsec, mtl, parser-combinators, case-insensitive, scientific →
  simplify by deletion only.** Do not rewrite these libraries to avoid a
  type system feature. Delete the modules and functions that aihc does not
  use, and keep the remaining code as it is upstream. aihc-parser defines a
  `Stream` instance with associated types, and the fc and grin parsers of
  aihc use the `MonadParsec` instance of `ReaderT`. So aihc also compiles
  unmodified only when megaparsec keeps its classes.

Changes to aihc itself (dropping dependencies, replacing the Custom setup)
are made in `vendor/aihc*` and should also go upstream where they make sense,
so re-vendoring a newer aihc stays cheap.

## Compiler interface

The tracker talks to aihc-boot only through this command-line contract, so
it does not depend on how aihc-boot is implemented. The command comes from
`[compiler].command` in `boot.toml` (default `./build/aihc-boot`), or from
the `AIHC_BOOT` environment variable. While the command does not exist,
M3–M7 read 0.

- `aihc-boot check --stage {parse|resolve|typecheck} --package NAME`
  checks every module of `vendor/NAME/` up to the given stage and prints one
  JSON object per module on stdout:
  `{"file": "vendor/NAME/src/Data/Foo.hs", "ok": true}`.
  Extra fields (such as `"error"`) are ignored. Modules missing from the
  output count as failures, so a crash only loses the modules after it.
- **A module counts as parsed only after a round trip through
  aihc-parser.** aihc-boot parses the module, prints its syntax tree back
  as source (with explicit braces), and runs `aihc-parse`
  (`tools/aihc-parse`, a driver for the vendored `aihc-parser`, the
  parser aihc itself uses) on both files. The module passes when the two
  trees are equal, source spans aside. This keeps the parser honest: a
  construct it skips or reorders shows up as a difference. `aihc-parse`
  comes from the Nix shell; `AIHC_PARSE` overrides its path. Without it
  every module fails.
- **Pragmas are ignored.** `INLINE`, `SPECIALIZE`, `UNPACK`, `SCC`,
  `SOURCE` and the others do not change what the vendored code
  computes. The lexer drops them, and `aihc-parse` removes them from
  aihc-parser's tree before the comparison. Only `LANGUAGE` stays,
  because it changes how a module is read.
- **A module counts as resolved only when aihc-resolve agrees.** The
  reference is `resolve-oracle` (`tools/resolve-oracle`), a small program
  on the vendored `aihc-resolve` library. It resolves the package and its
  vendored dependencies with the same source files, the same `.cabal`
  language settings and the same boot libraries that aihc-boot sees, so
  every top-level name resolves to the same package, module and name on
  both sides. Both sides print one record per identifier occurrence, and
  the module passes when the records agree: the same spans, the same
  namespaces, equal top-level targets, and local binders that partition
  the spans the same way (the numbering of locals is not compared). The
  oracle also comes from the Nix shell; `AIHC_RESOLVE_ORACLE` overrides
  its path. `aihc-boot manifest`, `dump` and `oracle` show the three
  inputs of the comparison. aihc-resolve takes every primitive from the
  outside: the manifest's `builtin` lines name the modules whose exports
  are in scope without an import. The syntax uses the list constructor
  `:` and the terms that desugaring applies (`fromInteger`, `>>=` and the
  others) from these modules. When `base` is a dependency, the manifest
  names `GHC.Types`. The boot `base` does not define the desugaring terms
  yet.
- **The boot `base`** is in `vendor/base`. It is a small package that
  has only the names that the vendored tree uses. Its modules and names
  follow GHC's `base`, so the vendored tree also builds with GHC. GHC
  does not build `vendor/base`, so it is not in `cabal.project`. A
  primitive type is an empty `data` declaration (`data Int`). A
  primitive function is a `foreign import prim` declaration, which
  the evaluator implements, or a definition in Haskell (`seq` uses a
  bang pattern). The `foreign import prim` declarations are in
  `GHC.Prim`, or in the module that defines their primitive type when
  GHC has that module (`Integer` is in `GHC.Num.Integer`). Until the
  typechecker exists, the base has no classes: `+`, `==` and `show`
  work on `Int`, the Char comparisons are `eqChar` and the others from
  `GHC.Classes`, and `Integer` has the functions of `GHC.Num.Integer`
  (`integerAdd` and the others). Add
  names to it when a package does not resolve or an eval test needs
  them.
- Resolving and typechecking a package needs its dependencies, so aihc-boot
  reads the `.cabal` files under `vendor/` itself to find them. A
  dependency counts only when it is vendored.
- `aihc-boot run FILE.hs` runs a single-module program against the
  boot `base`. The eval tests use it. The program's standard input and
  output are the ones of `aihc-boot`. `AIHC_BOOT_HEAP` sets the initial
  heap in words; the tests use a small value to run the collector
  often.
- `aihc-boot core FILE.hs` prints the Core of the program's module, and
  `aihc-boot core --all FILE.hs` the Core of every module it reaches.
  For debugging and the golden tests in `tests/core/`.
- Stage-1 scripts invoke `$AIHC_BOOT` however they need to.

## Eval strategy

aihc-boot runs `aihc` with an interpreter. It does not generate code.
The interpreter is a lazy graph reducer over a small untyped Core
language, with a copying garbage collector. Speed matters little for a
one-off bootstrap build, but `boot.toml` gives stage 1 one hour. Measure
the time early.

The vendored tree uses type classes everywhere. Many methods are
resolved by their return type alone: `pure` and `>>=` in a `do` block
over `StateT`, `fromInteger` on a literal, `mempty`, `minBound` and
`read`. No run-time rule can select these dictionaries. So eval needs
dictionary-passing desugaring, and that needs type inference. The
typechecker is on the path to M6.

### Type system features

The rule is: keep the library code, and support the features it uses.
Deletion of unused code is permitted. A rewrite of a library to avoid a
feature is not. megaparsec sets the feature set. The examples come
from megaparsec 9.8.3, the current version on Hackage:

| Feature | Example in the target |
| --- | --- |
| Multi-parameter classes with functional dependencies | `class MonadParsec e s m \| m -> e s`, the mtl classes |
| Associated type synonyms | `Token s` and `Tokens s` in `Stream`, `Rep a` in `Generic` |
| Equality constraints, also as given constraints | `newline :: (MonadParsec e s m, Token s ~ Char) => m (Token s)` |
| Rank-N types in signatures and constructor fields | `newtype ParsecT e s m a = ParsecT { unParser :: forall b. ... }`, `runST` |
| `$` with a polymorphic argument | `ParsecT $ \s cok cerr eok eerr -> ...` |
| Class constraints on type family applications | `deriving instance (Show (Token s), Show e) => Show (ParseError s e)` |
| Flexible and undecidable instances | `instance (Stream s, MonadState st m) => MonadState st (ParsecT e s m)` |
| Default signatures | `default rnf :: (Generic a, GNFData (Rep a)) => a -> ()` |
| Scoped type variables, type applications | `Proxy :: Proxy s` in `Stream` |
| Deriving `Eq`, `Ord`, `Show`, `Enum`, `Bounded`, `Functor`, `Generic`, `NFData`, `Data`, `Typeable` | `ParseError`, `SourcePos`, the aihc syntax trees |

The source syntax also uses `OverloadedStrings`, `BangPatterns`,
`PatternSynonyms`, `LambdaCase`, `ViewPatterns` and `TupleSections`.
"Undecidable" instances only turn off a termination check, so they cost
nothing.

These features are not in the target, and the typechecker does not
support them: GADTs, data families, closed type families, `DataKinds`,
`PolyKinds` and injectivity annotations. Before the typechecker design
is final, vendor megaparsec and its dependencies, and count the
extensions and constructs over the whole tree. Many packages use
`GHC2021`, which turns on extensions without a pragma. A search for
`LANGUAGE` pragmas does not find these uses. Thus the count must use
the `.cabal` settings, or a check with GHC with a reduced extension set.

### Core

Core is untyped. It has lambda, application, `let` and `letrec`, `case`
on constructors and literals, literals and primitive calls. Both the
desugarer and the typechecker produce Core. The interpreter runs only
Core. This separation makes the evaluator and the typechecker
independent: one person can work on each. Name resolution (M4) is on
the critical path for both.

### Steps

Each step has tests that pass or fail without human input. Do the steps
in this order, but the evaluator steps (1, 2) and the typechecker step
(3) can run at the same time.

1. **Evaluator.** Desugar a monomorphic subset of Haskell into Core
   with no typechecker: no classes, no overloaded literals, only `Int`,
   `Char`, lists, tuples and `putStrLn`. Write the evaluator and the
   collector in C. Eval tests in `tests/eval/base/NAME.hs` check
   laziness, recursion, pattern matching, guards, `where` and `let`.
   GHC writes the golden `NAME.stdout`.
2. **Primitives.** Add `Int`, `Integer`, `Double`, `Char`, `IORef`,
   `MVar`, `STRef`, exceptions, and file and process IO as interpreter
   builtins. The tree uses mutable references and exceptions in many
   places, so these are not optional. Add a name to `vendor/base` when
   an eval test needs it.
3. **Typechecker.** A constraint-based typechecker, a reduced form of
   OutsideIn(X) as in GHC, for the features in "Type system features".
   It generates wanted constraints and solves them against given
   constraints from signatures, instance contexts and class contexts.
   The solver handles class constraints, equalities and type family
   reduction. It improves types with functional dependencies. It
   uses rigid type variables for signatures and rank-N fields, and it
   checks expressions against their signatures. A special rule for `$`
   lets a polymorphic argument through, as in older versions of GHC. It
   also handles superclasses, defaulting and the `deriving` clauses of
   the table. Design the solver for all of these features from the
   start. A Hindley-Milner checker with type families added later
   costs more. The vendored `aihc-tc` (`aihc-tc/src/Aihc/Tc/Solve/`)
   solves the same problems and is a reference for the design. The
   output of the typechecker is Core with explicit dictionaries, so it
   plugs into the evaluator of step 1 without a change. M5 counts a
   module as typechecked when aihc-boot accepts it. A wrong dictionary
   shows up as an eval failure. Add an oracle on the vendored `aihc-tc`,
   in the style of the parse and resolve oracles, only if the accept
   rate hides bugs.
4. **Per-package eval tests.** `deepseq`, then `containers`, `text`,
   `bytestring`, `filepath`, `transformers`, `megaparsec`,
   `prettyprinter`, then the `aihc-*` packages. Each package gives one
   visible "package works" step and grows the boot `base`.
5. **Whole-tree load.** Load every vendored module into one Core
   program. Check memory and time. This step tests the collector and
   the size of `aihc`. One known cost comes first: the evaluator's
   environments are linked frames, so a closure keeps every variable of
   its enclosing frames alive, not only the ones it uses. A function
   that walks a list from a frame that also holds the list's head keeps
   the whole list in memory. Flat closures, which capture only their
   free variables, remove this leak.
6. **Stage 1.** The `tests/stage1` ladder: link, `--version`, hello
   world, a subset of the tests of aihc.

Two rules help at every step. First, keep the "not supported yet" error
with a position in the desugarer and the typechecker, so the tracker
always shows the next construct to implement. Second, prune `aihc`
itself. `Read`, `Data` and `Generic` deriving clauses appear on about
one hundred types each. Delete the clauses that no code uses. The
typechecker supports associated types, so `deriving Generic` is not
expensive: the boot `GHC.Generics` has `Rep` without metadata, and a
derived instance only gives `Rep`, `from` and `to`.

### The boot base and the core libraries of aihc

aihc has its own core libraries in `core-libs/`: `aihc-prim`,
`aihc-internal` and `aihc-base`. aihc-boot does not run them. It has
its own small `base` in `vendor/base`, and takes from the aihc
libraries what is useful.

| Code | Modules | Lines |
| --- | ---: | ---: |
| aihc-prim + aihc-internal + aihc-base | 503 | about 37,700 |
| aihc itself, the whole target | 94 | 46,705 |

The aihc libraries would almost double the code that aihc-boot has to
compile, and in the hardest dialect in the tree. `aihc-prim` mirrors
GHC: `data Int = I# Int#`, IO as a function on a state token, unboxed
tuples, levity-polymorphic `seq` and `raise#`, `DataKinds`, `PolyKinds`,
kind signatures and about four hundred `foreign import prim` primops.
An interpreter wants the opposite: `Int` is a machine integer in a heap
cell, and `+` is one builtin. The same reduction that took `text` from
twenty thousand lines to one thousand applies to `base`.

Take these parts from the aihc libraries:

- **Module names and export lists**, so the vendored tree continues to
  build with the real `base` of GHC.
- **Pure Haskell definitions** for `Data.List`, `Data.Maybe`,
  `Control.Monad`, the `Foldable` and `Traversable` instances, and the
  `Show` and `Read` machinery. They are plain code, and GHC has the
  same code.
- **The `foreign import prim` declaration form** for the boxed
  primitives of the boot `base`. `aihc-parser` and `aihc-resolve` read
  it, and GHC does not build `vendor/base`, so it costs nothing.

There are two different bases. The boot `base` is the library that
aihc-boot runs `aihc` against. The `core-libs` of aihc are the input
that a stage-1 `aihc` compiles hello world against. M7 vendors
`core-libs` as data for `aihc` to read, but aihc-boot never interprets
them. `aihc-rts` is the native and wasm runtime for GRIN output, and
the interpreter does not use it.

## Open decisions

1. **Implementation language of aihc-boot: C89.** Decided. A non-Haskell
   implementation makes aihc-boot a real bootstrap path (no GHC needed).
   ISO C89 is the simplest language with a compiler on almost every
   system, so the bootstrap needs nothing more than a C compiler. The
   first version was in Rust; the C version replaced it with the same
   behavior. See "Layout of the compiler" below.
2. **Code generation target for M6/M7: an interpreter.** Decided. aihc-boot
   interprets Core and generates no code. It is the smallest option, and
   slow is fine for a one-off bootstrap build. See "Eval strategy" above.
3. **How far to simplify `text`:** keep a real UTF-8 array implementation,
   or back `Text` with `String` and accept the slower stage-1 compiler.
4. **Whether `vector`/`primitive` become thin wrappers over `array`.**

## Layout of the compiler

aihc-boot is written in ISO C89 (`-std=c89 -pedantic`) and uses only the
C standard library, with one exception: `src/sys.c` wraps the POSIX
calls for directories and child processes. The sources are in `src/`,
one file for each stage:

- `src/util.c`: memory, growable arrays (`Vec`), string builders
  (`StrBuf`), strings, Unicode character classes and files.
  `src/unicode_tables.c` holds the Unicode range tables; it comes from
  `scripts/unicode_tables.py`.
- `src/syntax.h`: the tokens and the syntax tree. `src/lexer.c`,
  `src/layout.c`, `src/parse*.c` (one file for the module header,
  declarations, expressions, patterns and types), `src/print.c` and
  `src/debug.c` (the `aihc-boot parse` dump) implement the syntax
  stages. The parser skips nothing: a construct it does not know is a
  parse error with a position. Operator chains stay flat and in source
  order; fixity resolution comes after name resolution. Every name in
  the tree has a span. Spans do not take part in the comparison of two
  trees.
- `tools/aihc-parse`: the reference parser, a small driver for the
  vendored `aihc-parser`. See "Compiler interface" above.
- `src/resolve.h` and the files it names: name resolution. They define
  the resolution records, read and write them, and compare two
  resolutions of a module. The resolver gives every identifier
  occurrence a target. It uses the span rules of aihc-resolve, because
  the comparison needs the same spans. The resolver handles the
  constructs of `base` and `deepseq`. A module with another construct
  fails with a "not supported yet" error and the position of the
  construct, so `aihc-boot check --stage resolve --package NAME` shows
  which construct to implement next.
- `tools/resolve-oracle`: the reference resolver, a small program on the
  vendored `aihc-resolve`. See "Compiler interface" above.
- `src/manifest.c`: reads the `.cabal` files under `vendor/` and writes
  the manifest that the oracle reads.
- `src/core.h` and `src/core.c`: Core, the untyped language of the
  interpreter (see "Eval strategy"), and its printer. A variable is a
  frame distance and a slot, not a name. A lambda, a let group and a
  case alternative with fields each bind one frame.
- `src/desugar.c`: the syntax tree of every module that Main reaches to
  Core. It takes the top-level scopes from `src/scope.c` and keeps the
  local scope itself. Fixity resolution follows section 10.6 of the
  Haskell 2010 report. Pattern matching becomes nested `case`
  expressions with a fail expression for the next clause, the next
  alternative or the next guard. The clauses after the first are
  let-bound, so a failed match evaluates each one at most once. A
  newtype constructor is erased. A construct outside the subset fails
  with a "not supported yet" error and the position of its declaration.
- `src/eval.c`: the evaluator. A lazy abstract machine with an explicit
  stack of continuation frames (update, argument, case, primitive) over
  a heap of words, and a copying collector that doubles the heap when
  it fills up. The primitives are in a table by name. A `foreign import
  prim "NAME" f :: t` declaration in the boot `base` binds `f` to the
  primitive NAME; the evaluator defines the arity and the strictness of
  each primitive. IO is a function from a world token to `IORes a`;
  `main` runs applied to the token. Primitives that need a whole
  String force it through a nested run of the machine. An `Integer`
  that fits in a C `long` is an `Int` object. A larger `Integer` is a
  sign and limbs of 16 bits, so the arithmetic needs no type wider
  than C89 has.
- Exceptions: the `raise` primitive jumps (`longjmp`) to the innermost
  run of the machine. That run pops stack frames until it finds the
  frame of a `catch` primitive, and then applies the handler. A thunk
  under evaluation on the way becomes a thunk again. A nested run
  passes the exception on to the run that called it. The outermost run
  prints an uncaught exception and stops with exit code 1. The boot
  `base` has no `Exception` class yet, so an exception is the thrown
  value itself, and a handler gets every exception. `SomeException` is
  a newtype, so its pattern matches every exception. A program that
  catches only exceptions of the handler's type gets the same result
  as with GHC. The typechecker will add the class and the selection by
  type.
- `src/main.c`: the `aihc-boot` command. It implements the command line
  contract above and reports every module as JSON. Failures carry an
  `error` field and, when known, the `line` and `col` of the first
  error, so `aihc-boot check --stage parse --package NAME` also shows
  which construct to implement next.

aihc-boot is a short-lived process, so it does not free most of its
memory. The tests are in `tests/`: golden files for the syntax stages
(`tests/syntax/`) and for Core (`tests/core/`), the eval tests
(`tests/eval/`), and a round trip (print, parse, print) over every
vendored module that parses.

## Tooling

- `nix develop` gives the full dev shell (Python for the scripts, GHC 9.12
  and cabal as the reference compiler, a C compiler and make for aihc-boot). `nix develop .#tracker` is the
  small shell the scheduled workflow uses.
- `scripts/vendor.py NAME` / `--all` fetches upstream sources into
  `vendor/` and records the version and baseline size in `vendor/lock.json`.
- `cabal build all` in the dev shell builds the vendored packages in
  `cabal.project` with GHC. The local boot libraries (`bytestring`,
  `containers` and the others) replace the copies that come with GHC.
  Add a package to `cabal.project` when all of its dependencies are
  vendored.
- `scripts/progress.py` prints the report; `--write` updates the README and
  `progress/history.csv`. It records a new history row only when some number
  changed, so the daily run commits nothing on days without progress.
- `make` builds `build/aihc-boot`, the path the tracker looks for. Any
  C89 compiler works: `mkdir -p build && cc -o build/aihc-boot src/*.c`
  is enough.
  `make check` runs the tests. `sh tests/run.sh --update` writes new
  golden files after an intended change of output.
- `nix flake check` runs the tracker on a clean checkout, and builds
  and tests aihc-boot. The build uses `-Werror`, so a warning fails it.
- The progress workflow pushes to `main` with a deploy key, because a
  ruleset cannot bypass `GITHUB_TOKEN`. To set up the key:
  1. Run `ssh-keygen -t ed25519 -N "" -C progress -f progress_key`.
  2. Add `progress_key.pub` as a deploy key with write access (Settings →
     Deploy keys).
  3. Add the contents of `progress_key` as the repository secret
     `PROGRESS_DEPLOY_KEY` (Settings → Secrets and variables → Actions).
  4. Add "Deploy keys" to the bypass list of the ruleset on `main`.
  5. Delete the two local key files.
