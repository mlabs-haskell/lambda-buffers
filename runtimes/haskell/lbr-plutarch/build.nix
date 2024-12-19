{ inputs, ... }:
{
  perSystem = { config, pkgs, system, ... }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellPlutusFlake {
        src = ./.;

        name = "lbr-plutarch";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          "${inputs.plutarch}"
          "${inputs.plutarch}/plutarch-ledger-api"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in

    {
      devShells.dev-lbr-plutarch = hsFlake.devShell;

      packages = {
        lbr-plutarch-lib = hsFlake.packages."lbr-plutarch:lib:lbr-plutarch";
        lbr-plutarch-src = pkgs.stdenv.mkDerivation {
          name = "lbr-plutus-haskell-src";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

        lbr-plutarch-tests = hsFlake.packages."lbr-plutarch:test:tests";
      };

      checks.check-lbr-plutarch = hsFlake.checks."lbr-plutarch:test:tests";

    };
}
