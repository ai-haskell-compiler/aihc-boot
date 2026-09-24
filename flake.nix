{
  description = "aihc-boot: a minimal Haskell compiler whose only job is to build aihc";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = {
    self,
    nixpkgs,
  }: let
    systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

    # Just enough to run the progress tracker (used by the scheduled workflow).
    trackerTools = pkgs: [pkgs.python3 pkgs.git pkgs.bash];
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
            pkgs.haskell.compiler.ghc912
            pkgs.cabal-install
            pkgs.curl
          ];
      };
      tracker = pkgs.mkShell {packages = trackerTools pkgs;};
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
      # The tracker must run on a clean checkout without the boot compiler.
      progress = pkgs.runCommand "progress-check" {nativeBuildInputs = trackerTools pkgs;} ''
        cp -r ${self} src && chmod -R u+w src && cd src
        python3 scripts/progress.py --no-compiler > $out
      '';
    });
  };
}
