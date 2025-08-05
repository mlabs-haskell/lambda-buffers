{ inputs, ... }:
{
  perSystem =
    { config, system, ... }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellPlutusFlake {
        src = ./.;

        name = "plutustx-example";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          # LB PlutusTx backend imports
          "${config.packages.lbf-prelude-plutustx}"
          "${config.packages.lbf-plutus-plutustx}"
          "${config.packages.lbr-plutustx-src}"

          # LB Haskell backend imports (Prelude and Plutus)
          "${config.packages.lbr-prelude-haskell-src}"
          "${config.packages.lbf-prelude-haskell}"
          "${config.packages.lbr-plutus-haskell-src}"
          "${config.packages.lbf-plutus-haskell}"

          # Api
          "${config.packages.lbf-plutustx-example-api}"

        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in

    {
      devShells.dev-plutustx-example = hsFlake.devShells.default;

      packages = {
        # Derivation for the .lbf schema file
        lbf-plutustx-example-api = config.lbf-nix.lbfPlutusTx {
          name = "lbf-plutustx-example-api";
          src = ./api;
          files = [ "Example.lbf" ];
        };

        # An example application which uses the generated PlutusTx types
        plutustx-example-cli = hsFlake.packages."plutustx-example:exe:plutustx-example";

      };
    };
}
