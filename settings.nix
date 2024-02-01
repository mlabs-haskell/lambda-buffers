# Repo wide settings
{ lib, flake-parts-lib, inputs, ... }: {

  options = {

    perSystem = flake-parts-lib.mkPerSystemOption
      ({ system, config, pkgs, ... }: {
        options.settings = {

          shell = {

            tools = lib.mkOption {
              type = lib.types.listOf lib.types.package;
              description = "Tools to include in all devShells";
            };

            hook = lib.mkOption {
              type = lib.types.str;
              description = "Shell script to invoke in all devShells";
            };
          };

          haskell = {

            index-state = lib.mkOption {
              type = lib.types.str;
              description = "Hackage index state to use when making a haskell.nix build environment";
            };

            compiler-nix-name = lib.mkOption {
              type = lib.types.str;
              description = "GHC Haskell compiler to use when building haskell.nix projects";
            };

          };

        };


        config = {

          settings = {

            haskell = {
              index-state = "2024-01-16T11:00:00Z";
              compiler-nix-name = "ghc963";
            };

            shell = {

              tools = [

                pkgs.haskellPackages.apply-refact

                pkgs.nil
                inputs.pre-commit-hooks.outputs.packages.${system}.deadnix
                inputs.pre-commit-hooks.outputs.packages.${system}.nixpkgs-fmt

                inputs.pre-commit-hooks.outputs.packages.${system}.shellcheck

                inputs.pre-commit-hooks.outputs.packages.${system}.markdownlint-cli
                inputs.pre-commit-hooks.outputs.packages.${system}.dhall

                inputs.pre-commit-hooks.outputs.packages.${system}.purty
              ];

              hook = ''
                export LC_CTYPE=C.UTF-8;
                export LC_ALL=C.UTF-8;
                export LANG=C.UTF-8;
                ${config.pre-commit.installationScript}
              '';
            };
          };
        };

      });

  };

}
