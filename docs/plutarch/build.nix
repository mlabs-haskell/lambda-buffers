{ inputs, ... }:
{
  perSystem = { pkgs, config, ... }:
    let
      project = { lib, ... }: {
        src = ./.;

        name = "plutarch-example";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        extraHackage = [
          # Load Plutarch support
          "${config.packages.lbf-prelude-plutarch}"
          "${config.packages.lbf-plutus-plutarch}"
          "${config.packages.lbr-plutarch-src}"
          # Api
          "${config.packages.lbf-plutus-golden-api-plutarch}"
          "${config.packages.lbf-plutarch-example-api}"
          # Plutarch itself
          "${inputs.plutarch}"
          "${inputs.plutarch}/plutarch-extra"
        ];

        modules = [
          (_: {
            packages = {
              allComponent.doHoogle = true;
              allComponent.doHaddock = true;

              # Enable strict compilation
              plutarch-example.configureFlags = [ "-f-dev" ];
            };
          })
        ];

        shell = {

          withHoogle = true;

          exactDeps = true;

          nativeBuildInputs = config.settings.shell.tools ++ [ config.packages.lbf-plutus-to-plutarch ];

          tools = {
            cabal = { };
            haskell-language-server = { };
          };

          shellHook = lib.mkForce config.settings.shell.hook;
        };
      };
      hsNixFlake = (pkgs.haskell-nix.cabalProject' [
        inputs.mlabs-tooling.lib.mkHackageMod
        inputs.mlabs-tooling.lib.moduleMod
        project
      ]).flake { };

    in

    {
      devShells.dev-plutarch-example = hsNixFlake.devShell;

      packages = {
        plutarch-example-cli = hsNixFlake.packages."plutarch-example:exe:plutarch-example";

        lbf-plutarch-example-api = config.overlayAttrs.lbf-nix.lbfPlutarch {
          name = "lbf-plutarch-example-api";
          src = ./api;
          files = [ "Example.lbf" ];
        };

      };


    };
}
