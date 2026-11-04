# aihc-boot plan

aihc-boot is a minimal Haskell compiler with one job: compile
[aihc](https://github.com/ai-haskell-compiler/aihc) and the vendored,
simplified dependencies aihc needs. It is not a general Haskell compiler.
Anything aihc does not use, aihc-boot does not have to support.

## Strategy

1. **Pin the target.** Vendor aihc and its whole dependency closure at fixed
   revisions (`boot.toml`, `vendor/lock.json`). The vendored tree defines the
   language aihc-boot has to understand.
2. **Keep the upstream code.** Vendor every dependency as upstream wrote
   it. aihc-boot supports the language features and the C code that the
   upstream code uses. Delete code only under the rule in "Upstream
   code" below. Patch aihc to drop dependencies that a boot build does
   not need, such as the network download.
3. **Keep GHC as the reference.** The vendored tree must keep building and
   passing its tests with GHC (the nix dev shell provides GHC 9.12 and
   cabal). This is what makes step 2 safe: a real compiler checks every
   deletion before aihc-boot has to handle the code.
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
| `simplify` | Upstream package, kept as upstream wrote it. Deletions follow the rule in "Upstream code" | `vendor/<name>/` (fetched, then edited) |
| `replace` | Small in-tree package with the same name and modules, covering only what aihc uses | `vendor/<name>/` (written by hand) |
| `drop` | Patch the packages that use it so the dependency goes away | nowhere |

The current plans are a first pass from aihc's `build-depends` and imports
at the pinned revision. The larger choices:

- **base / ghc-prim → replace.** aihc-boot ships its own small `base`,
  grown only as the vendored tree needs it. It uses the representation
  of GHC (`GHC.Exts`), because the upstream packages use it. See "The
  boot base and the core libraries of aihc". This is the largest single
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
- **async → drop.** A boot-built aihc can build sequentially. stm stays,
  because exceptions needs it.
- **haskeline → drop** (in `build-depends` but never imported), **libffi →
  drop** (only the GRIN interpreter uses it).
- **The other libraries → simplify, as upstream wrote them.** This
  includes text, bytestring, containers, transformers, megaparsec, mtl,
  parser-combinators, case-insensitive, scientific, hashable,
  integer-logarithms, primitive and prettyprinter. Only scientific has
  code to delete: its `Lift` instance uses Template Haskell. Earlier
  commits cut some of these packages down further. See "Earlier cuts".

Changes to aihc itself (dropping dependencies, replacing the Custom setup)
are made in `vendor/aihc*` and should also go upstream where they make sense,
so re-vendoring a newer aihc stays cheap.

## Upstream code

Keep the vendored code as upstream wrote it. aihc-boot supports the
features that the code uses. Do not rewrite a library to avoid a
feature.

Delete code only when both of these conditions are true:

1. No vendored package uses the code.
2. The code needs a feature from the excluded list.

The excluded list has one item: **Template Haskell**. This includes
splices, quotes, `Lift` instances and the `template-haskell` package.

These features are supported. They are never a reason to delete code:

- GHC internals: `GHC.Exts`, `MagicHash`, unboxed tuples, primitive
  types such as `Int#` and `State#`, and primops.
- C code: `foreign import ccall` and `capi`, and the C files in
  `cbits/`. See "C code".
- All type system features that the code uses, for example
  `QuantifiedConstraints` in the `MonadTrans` class of transformers.

Follow these steps when you vendor a package:

1. Use the upstream cabal flags. A flag is not a change of the code.
   Turn off a flag only when it selects code in C++ or in another
   language that is not C. For example, turn off the `simdutf` flag of
   text.
2. Delete whole modules, instances or functions. Then delete the
   imports, exports and `.cabal` entries that the deletion leaves
   unused. Do not edit the lines that remain.
3. Compare the result with the upstream tarball. `diff` must show only
   removed lines in the Haskell and C files.

Sometimes the used code needs an excluded feature. Then do not change
the library. Change the package that uses the code, or ask the
maintainers.

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
  does not build `vendor/base`, so it is not in `cabal.project`. Today
  a primitive type is an empty `data` declaration (`data Int`). This
  changes: the boot `base` moves to the representation of GHC, as "The
  boot base and the core libraries of aihc" describes. A
  primitive function is a `foreign import prim` declaration, which
  the evaluator implements, or a definition in Haskell (`seq` uses a
  bang pattern). The `foreign import prim` declarations are in
  `GHC.Prim`, or in the module that defines their primitive type when
  GHC has that module (`Integer` is in `GHC.Num.Integer`). The base has
  the classes of GHC's base (`Eq`, `Ord`, `Num`, `Show`, `Monad`,
  `Foldable` and the others). Its modules come in layers without import
  cycles, so a module below `GHC.Num` (such as `GHC.Show`) has no
  integer literals. `scripts/gen_base_ints.py` writes the fixed-size
  integer types (`GHC.Word`, `GHC.Int`, `Foreign.C.Types`). A `Ptr` is
  an address in an `Int`, and the base never frees memory. Add names to
  the base when a package does not resolve or an eval test needs them.
- Resolving and typechecking a package needs its dependencies, so aihc-boot
  reads the `.cabal` files under `vendor/` itself to find them. A
  dependency counts only when it is vendored.
- `aihc-boot run FILE.hs` runs a single-module program against the
  boot `base`, or against PKG and its dependencies for a file in
  `tests/eval/PKG/`. The eval tests use it. For a file under
  `vendor/PKG/`, the file is the main module of an executable of PKG.
  Thus `aihc-boot run vendor/aihc/exe/Main.hs` runs `aihc`. The
  program's standard input and
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
See "Upstream code" for the only deletions that are permitted. Most
examples come from megaparsec 9.8.3, the current version on Hackage:

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
| Quantified constraints | `class (forall m. Monad m => Monad (t m)) => MonadTrans t` in transformers |
| Primitive types, primops and unboxed tuples | `i2d (I# i#) = C# (chr# (ord# '0'# +# i#))` in scientific, `Data.Primitive.Array` |

The source syntax also uses `OverloadedStrings`, `BangPatterns`,
`PatternSynonyms`, `LambdaCase`, `ViewPatterns`, `TupleSections` and
`MagicHash`. "Undecidable" instances only turn off a termination check,
so they cost nothing.

megaparsec does not use these features: GADTs, data families, closed
type families, `DataKinds`, `PolyKinds` and injectivity annotations.
Other upstream packages, such as primitive, can use some of them. The
typechecker supports each feature that the vendored tree uses. Before
the typechecker design is final, vendor the dependency closure, and
count the
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
   an eval test needs it. Add calls to C, as "C code" describes.
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
   module as typechecked when it resolves as the oracle resolves it and
   aihc-boot accepts it. A wrong dictionary
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
compile. So the boot `base` stays small: it has only the names that the
vendored tree uses.

But the boot `base` uses the representation of GHC, as `aihc-prim`
does. The upstream packages use `GHC.Exts`: primitive, scientific,
hashable, text and bytestring all match on `I#` and `C#`, or call
primops. Upstream code stays as it is, so the boot `base` provides
these names:

- The boxed types as GHC defines them: `data Int = I# Int#`,
  `data Char = C# Char#` and the others.
- IO and ST as functions on a state token, with unboxed tuples:
  `newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))`.
- The primitive types (`Int#`, `Array#`, `MutableArray#`, `State#` and
  the others) and the primops that the vendored tree uses, as
  `foreign import prim` declarations in `GHC.Prim`.
- `GHC.Exts`, `GHC.Base` and the other internal modules, with the names
  that the vendored tree imports from them.

The interpreter can still keep an `Int#` as a machine integer, and a
primop such as `+#` as one builtin. Only the box (`I#`) is new.

Take these parts from the aihc libraries:

- **Module names and export lists**, so the vendored tree continues to
  build with the real `base` of GHC.
- **Pure Haskell definitions** for `Data.List`, `Data.Maybe`,
  `Control.Monad`, the `Foldable` and `Traversable` instances, and the
  `Show` and `Read` machinery. They are plain code, and GHC has the
  same code.
- **The `foreign import prim` declaration form** for the primops of
  the boot `base`. `aihc-parser` and `aihc-resolve` read it, and GHC
  does not build `vendor/base`, so it costs nothing.
- **The primitive types and the primop declarations** of `aihc-prim`.
  They follow GHC, as the boot `base` now does.

There are two different bases. The boot `base` is the library that
aihc-boot runs `aihc` against. The `core-libs` of aihc are the input
that a stage-1 `aihc` compiles hello world against. M7 vendors
`core-libs` as data for `aihc` to read, but aihc-boot never interprets
them. `aihc-rts` is the native and wasm runtime for GRIN output, and
the interpreter does not use it.

### CPP

About one hundred upstream modules use the C preprocessor: `#if`,
`#include` and `MIN_VERSION_pkg(a,b,c)`. For now, aihc-boot runs clang
as the preprocessor, as GHC runs `cc -E -undef -traditional`
(`src/cpp.c`). `AIHC_CPP` overrides the command. A module goes through
clang when a `LANGUAGE` pragma or the `default-extensions` of its
package turns on `CPP`. aihc-boot defines the macros that GHC 9.12 and
Cabal define:

- `__GLASGOW_HASKELL__` (912) and `MIN_VERSION_GLASGOW_HASKELL`.
- `VERSION_pkg` and `MIN_VERSION_pkg` for the package of the module and
  its dependencies. A vendored package has the version of its `.cabal`
  file. base, ghc-prim and the other packages of GHC have the versions
  of GHC 9.12.
- The platform macros of the host, such as `darwin_HOST_OS` and
  `aarch64_HOST_ARCH`.
- The `cpp-options` and the `include-dirs` of the library in the
  `.cabal` file.

`cpp/include` has the GHC headers that the vendored code includes:
`MachDeps.h`, `ghcautoconf.h` and `HsBaseConfig.h`. The preprocessed
module goes to `build/cpp/PATH`. The lines of the module keep their
numbers. `aihc-parse` and `resolve-oracle` read the same file, so both
sides of a comparison see the same source.

A preprocessor in C89 can replace clang later. The vendored `aihc-cpp`
is the reference for it.

### C code

Upstream packages call C with `foreign import ccall` and `foreign
import capi`. Their C files are in `cbits/` or a similar directory, and
the `.cabal` file lists them in `c-sources`. aihc-boot runs this code:

1. A script reads every `foreign import ccall` and `capi` declaration in
   `vendor/`. It writes one C wrapper for each declaration, and a table
   of the wrappers by name.
2. The build compiles the wrappers and the `c-sources` of the vendored
   packages, and links them into `aihc-boot`.
3. The interpreter calls a wrapper through the table. The wrapper takes
   the arguments from the heap of the interpreter, calls the C
   function, and boxes the result.

aihc-boot has no libffi. So it can call only the C functions that the
build knows. A `foreign import "wrapper"` or `"dynamic"` declaration
needs more work. Add support for these only when a package uses them.

The rules for C in `AGENTS.md` (C89, only the C standard library)
apply to the sources of aihc-boot in `src/`. Vendored C code keeps the
dialect of its upstream. The boot C compiler (TinyCC) must compile it.
Check this for each package with C code. TinyCC does not compile C++,
so turn off upstream flags that select C++ code.

### Earlier cuts

Earlier commits vendored some packages under an older rule: cut each
package down to what aihc uses. These packages are:

- text and bytestring: cut down, built with the `pure-haskell` flag
  (no C code), and without unboxed tuples (commit `5b77c10`).
- containers, deepseq and filepath: cut down.

Vendor these packages again from upstream, one pull request for each,
and apply the rule in "Upstream code". Then remove the "Do not re-vendor
with --force" notes from `boot.toml`.

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
3. **How far to simplify `text`: not at all.** Decided. text stays as
   upstream wrote it, with its C code. The `simdutf` flag is off,
   because simdutf is C++. See "Upstream code".
4. **`vector` and `primitive`: upstream code.** Decided. They are not
   wrappers over `array`.
5. **The `replace` plans of aeson, Cabal-syntax, optparse-applicative
   and cryptohash-sha256.** These plans come from the older rule. Decide
   for each package if it stays `replace`, or if the upstream code is
   vendored. Some modules of aeson use Template Haskell. Before the
   decision, find out if aihc uses them.

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
  constructs of `base`, `deepseq`, `bytestring` and `text`. A module
  with another construct fails with a "not supported yet" error and the
  position of the construct, so `aihc-boot check --stage resolve --package NAME` shows
  which construct to implement next.
- `tools/resolve-oracle`: the reference resolver, a small program on the
  vendored `aihc-resolve`. See "Compiler interface" above.
- `src/manifest.c`: reads the `.cabal` files under `vendor/` and writes
  the manifest that the oracle reads.
- `src/core.h` and `src/core.c`: Core, the untyped language of the
  interpreter (see "Eval strategy"), and its printer. A variable is a
  frame distance and a slot, not a name. A lambda, a let group and a
  case alternative with fields each bind one frame.
- `src/tc.h` and `src/tc_*.c`: the typechecker (see step 3 of "Eval
  strategy"). `tc_type.c` has the types, unification and matching,
  `tc_solve.c` the constraints and the solver, `tc_expr.c` the
  expressions, patterns and binding groups, `tc_decl.c` the
  declarations and modules, and `tc_derive.c` the derived instances.
  Inference makes wanted constraints. Each wanted keeps the givens that
  were in scope where inference made it. A binding group without
  signatures is split into strongly connected components, and each
  component is generalized with levels. The typechecker writes notes
  into the syntax tree: the dictionaries of each overloaded occurrence,
  and the dictionary parameters of each binding. A derived instance is
  Haskell source with qualified names, which the typechecker parses
  and checks as a written instance. The typechecker does not check
  kinds or skolem escape: GHC checks the vendored tree.
- `src/fixity.c`: fixity resolution, as section 10.6 of the Haskell
  2010 report specifies. The typechecker and the desugarer use it.
- `src/desugar.c`: the syntax tree of every module that Main reaches to
  Core. It takes the top-level scopes from `src/scope.c` and keeps the
  local scope itself. It reads the notes of the typechecker: a class
  becomes a data type of dictionaries, an instance a global that builds
  a dictionary, and an overloaded name takes its dictionaries as
  arguments. Pattern matching becomes nested `case`
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
