{ inputs, ... }:
{
  imports = [
    inputs.proto-nix.lib.preCommitModule
    inputs.flake-lang.flakeModules.rustMonorepoPreCommit
  ];
  perSystem =
    { config, pkgs, ... }:
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
            nixfmt-rfc-style.enable = true;
            deadnix.enable = true;
            statix.enable = true;
            statix.settings.ignore = [ "**spago-packages.nix" ];
            # Re-enable cabal-fmt and hlint once their hooks reliably edit working-tree
            # files in place across all sub-projects.
            cabal-fmt.enable = false;
            fourmolu.enable = true;
            ormolu.settings.cabalDefaultExtensions = true;
            hlint = {
              enable = false;
              entry = "${pkgs.writeShellScript "hlint-fix" ''
                status=0

                for file in "$@"; do
                  ${pkgs.haskellPackages.hlint}/bin/hlint \
                    --refactor \
                    --refactor-options=-i \
                    --with-refactor=${pkgs.haskellPackages.apply-refact}/bin/refactor \
                    "$file" || status=$?
                done

                # Check the resulting files for suggestions requiring manual changes.
                ${pkgs.haskellPackages.hlint}/bin/hlint "$@" || status=$?
                exit "$status"
              ''}";
            };
            typos.enable = true;
            typos.settings = {
              ignored-words = [
                "substituters"
                "hask"
                "lits"
                "Nd"
                "anc"
                "eit"
              ];
              exclude = [
                "*.pdf"
                "*.png"
                "*.log"
                "fourmolu.yaml"
              ];
            };

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

        };
      };
    };
}
