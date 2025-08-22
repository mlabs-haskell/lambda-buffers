{ inputs, ... }:
{
  perSystem =
    { config, system, ... }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellPlutusFlake {
        src = ./.;

        name = "lbt-plutus-plutarch";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          # Load Plutarch Haskell support
          "${config.packages.lbf-prelude-plutarch}"
          "${config.packages.lbf-plutus-plutarch}"
          "${config.packages.lbr-plutarch-src}"
          # Load pure Haskell support
          "${config.packages.lbf-prelude-haskell}"
          "${config.packages.lbf-plutus-haskell}"
          "${config.packages.lbr-prelude-haskell-src}"
          "${config.packages.lbr-plutus-haskell-src}"
          # Golden api
          "${config.packages.lbf-plutus-golden-api-plutarch}"
          "${config.packages.lbf-plutus-golden-api-haskell}"
          # Golden data
          "${config.packages.lbt-plutus-golden-haskell}"
          # Plutarch itself
          "${inputs.plutarch}"
          "${inputs.plutarch}/plutarch-ledger-api"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in
    {
      devShells.dev-lbt-plutus-plutarch = hsFlake.devShells.default;

      packages = {
        lbt-plutus-plutarch-lib = hsFlake.packages."lbt-plutus-plutarch:lib:lbt-plutus-plutarch";
        lbt-plutus-plutarch-tests = hsFlake.packages."lbt-plutus-plutarch:test:tests";
      };

      checks.check-lbt-plutus-plutarch = hsFlake.checks."lbt-plutus-plutarch:test:tests";
    };
}
