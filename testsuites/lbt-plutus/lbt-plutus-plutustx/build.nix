{ inputs, ... }:
{
  perSystem =
    { config, system, ... }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellPlutusFlake {
        src = ./.;

        name = "lbt-plutus-plutustx";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          # LB PlutusTx backend imports
          "${config.packages.lbf-prelude-plutustx}"
          "${config.packages.lbf-plutus-plutustx}"
          "${config.packages.lbf-plutus-golden-api-plutustx}"
          "${config.packages.lbr-plutustx-src}"

          # LB Haskell backend imports (Prelude and Plutus)
          "${config.packages.lbr-prelude-haskell-src}"
          "${config.packages.lbf-prelude-haskell}"
          "${config.packages.lbr-plutus-haskell-src}"
          "${config.packages.lbf-plutus-haskell}"
          "${config.packages.lbf-plutus-golden-api-haskell}"
          "${config.packages.lbt-plutus-golden-haskell}"

          # Plutarch (just for script evaluation module)
          "${inputs.plutarch}"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in

    {
      devShells.dev-lbt-plutus-plutustx = hsFlake.devShells.default;

      packages = {
        lbt-plutus-plutustx-tests = hsFlake.packages."lbt-plutus-plutustx:test:tests";
      };

      checks.check-lbt-plutus-plutustx = hsFlake.checks."lbt-plutus-plutustx:test:tests";
    };
}
