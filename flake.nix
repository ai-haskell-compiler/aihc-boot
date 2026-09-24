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

    # Just enough to run the progress tracker (used by the scheduled
    # workflow). The reference parser is part of it, because M3 counts
    # modules through it.
    trackerTools = pkgs: [pkgs.python3 pkgs.git pkgs.bash (aihcParse pkgs)];

    # The boot compiler itself. `cargoLock` reads the committed Cargo.lock,
    # so the build is reproducible and works without network access.
    aihcBoot = pkgs:
      pkgs.rustPlatform.buildRustPackage {
        pname = "aihc-boot";
        version = "0.1.0";
        src = self;
        cargoLock.lockFile = ./Cargo.lock;
        nativeCheckInputs = [pkgs.clippy pkgs.rustfmt];
        # `cargo test` runs in checkPhase. Add the linter and the
        # formatter, so `nix flake check` enforces both.
        postCheck = ''
          cargo fmt --check
          cargo clippy --all-targets --offline -- -D warnings
        '';
      };
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
            # The boot compiler is written in Rust.
            pkgs.cargo
            pkgs.rustc
            pkgs.clippy
            pkgs.rustfmt
            pkgs.rust-analyzer
          ];
      };
      tracker = pkgs.mkShell {packages = trackerTools pkgs;};
    });

    packages = forAllSystems (pkgs: {
      default = aihcBoot pkgs;
      aihc-boot = aihcBoot pkgs;
      aihc-parse = aihcParse pkgs;
    });

    apps = forAllSystems (pkgs: {
      progress = {
        type = "app";
        program = toString (pkgs.writeShellScript "progress" ''
          exec ${pkgs.python3}/bin/python3 scripts/progress.py "$@"
        '');
      };
    });

    checks = forAllSystems (pkgs: {
      # Build, test, lint and format-check the Rust workspace.
      aihc-boot = aihcBoot pkgs;

      # The tracker must run on a clean checkout without the boot compiler.
      progress = pkgs.runCommand "progress-check" {nativeBuildInputs = trackerTools pkgs;} ''
        cp -r ${self} src && chmod -R u+w src && cd src
        python3 scripts/progress.py --no-compiler > $out
      '';
    });
  };
}
