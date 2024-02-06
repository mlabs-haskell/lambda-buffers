{ inputs, ... }: {
  imports = [
    inputs.proto-nix.lib.preCommitModule
    inputs.flake-lang.flakeModules.rustMonorepoPreCommit
  ];
  perSystem = { config, pkgs, ... }:
    {
      devShells.default = pkgs.mkShell {
        name = "dev-default";
        buildInputs = config.settings.shell.tools;
        shellHook = config.settings.shell.hook;
      };

      pre-commit = {
        settings = {
          excludes = [
            ".*spago-packages.nix$"
            "lambda-buffers-codegen/data/goldens/.*"
            "lambda-buffers-codegen/data/lamval-cases/.*"
            "experimental/archive/.*"
            "experimental/ctl-env/autogen/.*"
            "experimental/plutustx-env/autogen/.*"
            "lambda-buffers-frontend/data/goldens/good/work-dir/.*"
            "lambda-buffers-testsuite/lbt-prelude/goldens/.*"
          ];

          hooks = {
            nixpkgs-fmt.enable = true;
            deadnix.enable = true;
            statix.enable = true;
            cabal-fmt.enable = true;
            fourmolu.enable = true;
            shellcheck.enable = true;
            hlint.enable = true;
            typos.enable = true;
            markdownlint.enable = true;
            dhall-format.enable = true;
            purty.enable = true;
            rustfmt-monorepo.enable = true;
            denofmt = {
              enable = true;
              files = "(\\.ts$)|(^tsconfig?(-base)\\.json$)";
            };
            denolint = {
              enable = true;
              files = "\\.m?ts$";
            };
            protolint.enable = true;
            txtpbfmt.enable = true;
          };

          settings = {
            ormolu.cabalDefaultExtensions = true;
            statix.ignore = [ "**spago-packages.nix" ];
          };
        };
      };
    };
}
