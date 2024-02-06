{ inputs, ... }:
{
  perSystem = { config, system, ... }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellPlutusFlake {
        src = ./.;

        name = "plutarch-example";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          # Load Plutarch support
          "${config.packages.lbf-prelude-plutarch}"
          "${config.packages.lbf-plutus-plutarch}"
          "${config.packages.lbr-plutarch-src}"
          # Api
          "${config.packages.lbf-plutus-golden-api-plutarch}"
          "${config.packages.lbf-plutarch-example-api}"
          # Plutarch itself
          "${inputs.plutarch}"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };
    in

    {
      devShells.dev-plutarch-example = hsFlake.devShell;

      packages = {
        plutarch-example-cli = hsFlake.packages."plutarch-example:exe:plutarch-example";

        lbf-plutarch-example-api = config.lbf-nix.lbfPlutarch {
          name = "lbf-plutarch-example-api";
          src = ./api;
          files = [ "Example.lbf" ];
        };

      };


    };
}
