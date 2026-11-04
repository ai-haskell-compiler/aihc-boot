{
  description = "aihc-boot: a minimal Haskell compiler whose only job is to build aihc";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = {
    self,
    nixpkgs,
  }: let
    systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

    # The reference parser: aihc-parser, the parser aihc itself uses,
    # compiled from vendor/aihc-parser with a small driver.
    # `aihc-boot check --stage parse` runs it on the original module and on
    # the module printed from aihc-boot's own syntax tree, and counts the
    # module as parsed only when the two trees are equal.
    aihcParseGhc = pkgs: pkgs.haskell.packages.ghc912.ghcWithPackages (p: [p.megaparsec p.prettyprinter]);
    aihcParse = pkgs:
      pkgs.stdenv.mkDerivation {
        pname = "aihc-parse";
        version = "0.1.0";
        src = self;
        nativeBuildInputs = [(aihcParseGhc pkgs)];
        buildPhase = ''
          ghc -O -ivendor/aihc-parser/src -outputdir build -o aihc-parse tools/aihc-parse/AihcParse.hs
        '';
        installPhase = ''
          install -D aihc-parse $out/bin/aihc-parse
        '';
      };

    # The reference resolver: aihc-resolve from the vendored tree, run over
    # the vendored tree. `aihc-boot check --stage resolve` counts a module
    # as resolved only when its own resolution of the module is equal to
    # the oracle's. The two library packages come from `vendor/`, so the
    # oracle follows the pinned aihc revision. Their tests need packages
    # the tree does not have, so the tests are off.
    resolveOracle = pkgs: let
      hs = pkgs.haskell.packages.ghc912;
      aihc-parser = hs.mkDerivation {
        pname = "aihc-parser";
        version = "0";
        src = ./vendor/aihc-parser;
        isLibrary = true;
        isExecutable = false;
        doCheck = false;
        doHaddock = false;
        libraryHaskellDepends = with hs; [base bytestring containers deepseq megaparsec prettyprinter text];
        license = pkgs.lib.licenses.unlicense;
      };
      aihc-resolve = hs.mkDerivation {
        pname = "aihc-resolve";
        version = "0";
        src = ./vendor/aihc-resolve;
        isLibrary = true;
        isExecutable = false;
        doCheck = false;
        doHaddock = false;
        libraryHaskellDepends = with hs; [aihc-parser base containers deepseq text];
        license = pkgs.lib.licenses.unlicense;
      };
    in
      hs.mkDerivation {
        pname = "resolve-oracle";
        version = "0.1.0";
        src = ./tools/resolve-oracle;
        isLibrary = false;
        isExecutable = true;
        doHaddock = false;
        executableHaskellDepends = with hs; [aihc-parser aihc-resolve base bytestring containers text];
        license = pkgs.lib.licenses.mit;
      };

    # Just enough to run the progress tracker (used by the scheduled
    # workflow). The two reference tools are part of it, because M3 and M4
    # count modules through them. aihc-boot runs clang as the C
    # preprocessor for modules with the CPP extension.
    trackerTools = pkgs: [pkgs.python3 pkgs.git pkgs.bash pkgs.clang (aihcParse pkgs) (resolveOracle pkgs)];

    # The boot compiler itself: plain C89, built with make. `make check`
    # runs the golden tests and the round trip over the vendored tree.
    # The Makefile compiles with -std=c89 -pedantic -Werror, so the build
    # also enforces the language standard.
    aihcBoot = pkgs:
      pkgs.stdenv.mkDerivation {
        pname = "aihc-boot";
        version = "0.1.0";
        src = self;
        doCheck = true;
        checkTarget = "check";
        # The round trip in `make check` preprocesses the CPP modules.
        nativeCheckInputs = [pkgs.clang];
        installPhase = ''
          install -D build/aihc-boot $out/bin/aihc-boot
        '';
      };

    # The boot C compiler: TinyCC on musl, from nixpkgs' minimal-bootstrap.
    # The chain starts from a 181-byte machine-code seed (hex0) and builds
    # the assemblers, M2-Planet, GNU Mes, TinyCC and musl from source. No
    # other compiler binary is part of it. See docs/BOOTSTRAP.md.
    # nixpkgs has tinycc-musl for x86_64-linux and i686-linux. The flake
    # builds for x86_64-linux only.
    bootCCSystems = ["x86_64-linux"];
    hasBootCC = pkgs: builtins.elem pkgs.stdenv.hostPlatform.system bootCCSystems;

    # `tcc` links statically against musl. The wrapper is built with the
    # bootstrap bash, so the closure has no stdenv binaries.
    bootCC = pkgs: let
      mb = pkgs.minimal-bootstrap;
      tcc = mb.tinycc-musl;
    in
      mb.bash.runCommand "boot-cc-${tcc.compiler.version}" {} ''
        mkdir -p $out/bin
        cat > $out/bin/tcc <<EOF
        #!${mb.bash}/bin/bash
        # "tcc -ar" must keep -ar as the first argument.
        if [ "\$1" = "-ar" ]; then
          exec ${tcc.compiler}/bin/tcc "\$@"
        fi
        exec ${tcc.compiler}/bin/tcc -B${tcc.libs}/lib -static "\$@"
        EOF
        chmod 555 $out/bin/tcc
      '';

    # Compile and run a C89 program with the boot C compiler. This
    # derivation also uses only the bootstrap chain.
    bootCCCheck = pkgs: let
      mb = pkgs.minimal-bootstrap;
    in
      mb.bash.runCommand "boot-cc-check" {} ''
        ${bootCC pkgs}/bin/tcc -Wall -Werror -o hello ${./tests/boot-cc/hello.c}
        out_text=$(./hello)
        expected=$(printf 'sum=55\nerror 42\nhello from tcc')
        if [ "$out_text" != "$expected" ]; then
          echo "unexpected output:"
          echo "$out_text"
          exit 1
        fi
        echo "$out_text" > $out
      '';
  in {
    formatter = forAllSystems (pkgs: pkgs.alejandra);

    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        packages =
          trackerTools pkgs
          ++ [
            # Reference toolchain: the vendored tree must keep building with
            # GHC so every simplification can be checked against a real
            # compiler. Matches the GHC series aihc itself uses.
            (aihcParseGhc pkgs)
            pkgs.cabal-install
            pkgs.curl
            # The boot compiler is written in C89. mkShell already
            # provides a C compiler and make.
            pkgs.gnumake
          ]
          # The boot C compiler. It is only `tcc`: `cc` stays the stdenv
          # compiler, because GHC and cabal use it.
          ++ pkgs.lib.optional (hasBootCC pkgs) (bootCC pkgs);
      };
      tracker = pkgs.mkShell {packages = trackerTools pkgs;};
    });

    packages = forAllSystems (pkgs:
      {
        default = aihcBoot pkgs;
        aihc-boot = aihcBoot pkgs;
        aihc-parse = aihcParse pkgs;
        resolve-oracle = resolveOracle pkgs;
      }
      // pkgs.lib.optionalAttrs (hasBootCC pkgs) {
        boot-cc = bootCC pkgs;
      });

    apps = forAllSystems (pkgs: {
      progress = {
        type = "app";
        program = toString (pkgs.writeShellScript "progress" ''
          exec ${pkgs.python3}/bin/python3 scripts/progress.py "$@"
        '');
      };
    });

    checks = forAllSystems (pkgs:
      {
        # Build and test the boot compiler.
        aihc-boot = aihcBoot pkgs;

        # The tracker must run on a clean checkout without the boot compiler.
        progress = pkgs.runCommand "progress-check" {nativeBuildInputs = trackerTools pkgs;} ''
          cp -r ${self} src && chmod -R u+w src && cd src
          python3 scripts/progress.py --no-compiler > $out
        '';
      }
      // pkgs.lib.optionalAttrs (hasBootCC pkgs) {
        # The boot C compiler compiles and runs a C89 program.
        boot-cc = bootCCCheck pkgs;
      });
  };
}
