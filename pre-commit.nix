{ inputs, lib, ... }: {
  imports = [
    inputs.pre-commit-hooks.flakeModule # Adds perSystem.pre-commit options
  ];
  perSystem = { pkgs, system, inputs', config, ... }:
    {
      devShells.dev-pre-commit = config.pre-commit.devShell;
      devShells.default = config.pre-commit.devShell;

      pre-commit = {
        settings = {

          excludes = [
            "lambda-buffers-codegen/data/goldens/.*"
            "experimental/archive/.*"
            "experimental/ctl-env/autogen/.*"
            "experimental/plutustx-env/autogen/.*"
            "experimental/ctl-env/spago-packages.nix"
            "lambda-buffers-frontend/data/goldens/good/work-dir/.*"
            "docs/compiler-api.md"
            "lambda-buffers-testsuite/lbt-prelude/goldens/.*"
            "runtimes/purescript/lbr-prelude/spago-packages.nix"
            ".*/spago-packages.nix$"
          ];

          hooks = {
            nixpkgs-fmt.enable = true;
            cabal-fmt.enable = true;
            fourmolu.enable = true;
            shellcheck.enable = true;
            hlint.enable = true;
            # TODO: Enable hunspell
            typos.enable = true;
            markdownlint.enable = true;
            dhall-format.enable = true;
            purty.enable = true;
            rustfmt.enable = true;
          } // (inputs.protobufs-nix.lib.${system}.preCommitHooks { inherit pkgs; });

          settings.ormolu.cabalDefaultExtensions = true;
        };
      };
    };
}
