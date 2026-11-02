# Boot C compiler

aihc-boot uses a C89 compiler that Nix builds from source. The only
pre-compiled binary in the chain is `hex0-seed`. It has 181 bytes on
x86_64.

## The chain

The chain comes from `pkgs.minimal-bootstrap` in nixpkgs. That package
uses the work of the stage0-posix, GNU Mes and live-bootstrap projects.

| Stage | Function |
| --- | --- |
| `hex0-seed` | Converts hex text to bytes. Nix checks its output against a fixed hash. |
| `hex0`, `hex1`, `hex2` | Assemblers. `hex2` is also a linker. |
| `M0`, `cc_*`, `M2-Planet` | A macro assembler, then compilers for a subset of C. |
| `mescc-tools`, `kaem` | Assembler, linker and a minimal shell, compiled from C. |
| GNU Mes | A Scheme interpreter and MesCC, a C compiler in Scheme. |
| `tinycc-mes` | TinyCC, compiled by MesCC. |
| `musl`, `tinycc-musl` | musl libc, and TinyCC compiled again against musl. |

Source tarballs are fixed-output derivations. Their hashes are in nixpkgs.

## Use

The dev shell has `tcc` on `x86_64-linux`. The wrapper
links statically against musl. `cc` stays the stdenv compiler, because GHC
and cabal use it.

```
nix develop
tcc -o hello tests/boot-cc/hello.c
```

The flake has these outputs on `x86_64-linux`:

- `packages.<system>.boot-cc`: the `tcc` wrapper.
- `checks.<system>.boot-cc`: compiles and runs `tests/boot-cc/hello.c`.

Both derivations use only the bootstrap chain. They do not use stdenv.

## Check the full chain

`nix flake check` can get the chain from `cache.nixos.org`. To build
every stage from the seed, turn off substitutes:

```
nix build -L --option substitute false .#checks.x86_64-linux.boot-cc
```

The `Bootstrap` workflow (`.github/workflows/bootstrap.yml`) runs this
command. Start it by hand from the Actions tab.

## Limits

- The flake has the boot C compiler on `x86_64-linux` only. nixpkgs has
  `tinycc-musl` for `x86_64-linux` and `i686-linux`, and the flake does
  not build for `i686-linux`.
- TinyCC accepts more than C89. It does not reject C99 code.
- The chain trusts Nix, the Linux kernel and the 181-byte seed.
