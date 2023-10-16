# Repo wide settings
{ lib, flake-parts-lib, inputs, ... }: {

  options = {

    perSystem = flake-parts-lib.mkPerSystemOption
      ({ pkgs, system, config, ... }: {
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
              index-state = "2022-12-01T00:00:00Z";
              compiler-nix-name = "ghc925";
            };

            shell = {

              tools = [
                pkgs.haskell.packages.ghc924.fourmolu
                pkgs.haskellPackages.apply-refact
                inputs.pre-commit-hooks.outputs.packages.${system}.nixpkgs-fmt
                inputs.pre-commit-hooks.outputs.packages.${system}.cabal-fmt
                inputs.pre-commit-hooks.outputs.packages.${system}.shellcheck
                inputs.pre-commit-hooks.outputs.packages.${system}.hlint
                inputs.pre-commit-hooks.outputs.packages.${system}.typos
                inputs.pre-commit-hooks.outputs.packages.${system}.markdownlint-cli
                inputs.pre-commit-hooks.outputs.packages.${system}.dhall
                inputs.pre-commit-hooks.outputs.packages.${system}.purty
                inputs.pre-commit-hooks.outputs.packages.${system}.deadnix
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
