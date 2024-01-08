{ inputs, ... }: {
  imports = [
    ./extras/pre-commit-hooks-extra.nix
    inputs.proto-nix.lib.preCommitModule
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
            # TODO: Enable hunspell
            typos.enable = true;
            markdownlint.enable = true;
            dhall-format.enable = true;
            purty.enable = true;
            rustfmt-monorepo.enable = true;
            my-denofmt.enable = true;
            my-denolint.enable = true;
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
