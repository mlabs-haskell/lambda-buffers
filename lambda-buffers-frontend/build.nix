{ inputs, ... }:
{
  perSystem = { pkgs, config, ... }:
    let
      project = { lib, ... }: {
        src = ./.;

        name = "lambda-buffers-frontend";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        extraHackage = [
          "${config.packages.lambda-buffers-lang-hs-pb}"
          "${config.packages.lambda-buffers-compiler-hs-pb}"
          "${config.packages.lambda-buffers-codegen-hs-pb}"
          "${config.packages.lambda-buffers-compiler-src}"
        ];

        modules = [
          (_: {
            packages = {
              allComponent.doHoogle = true;
              allComponent.doHaddock = true;

              # Enable strict compilation
              lambda-buffers-frontend.configureFlags = [ "-f-dev" ];
            };
          })
        ];

        shell = {

          withHoogle = true;

          exactDeps = true;

          nativeBuildInputs = [
            config.packages.lbc
            config.packages.lbg
            config.packages.lbg-haskell
            config.packages.lbg-purescript
          ] ++ config.settings.shell.tools;

          tools = {
            cabal = { };
            haskell-language-server = { };
          };

          shellHook = lib.mkForce config.settings.shell.hook;
        };
      };
      hsNixFlake = (pkgs.haskell-nix.cabalProject' [
        inputs.mlabs-tooling.lib.mkHackageMod
        project
      ]).flake { };

    in

    {
      devShells = {
        dev-frontend = hsNixFlake.devShell;

        lb = pkgs.mkShell {
          name = "lambdabuffers-env";
          packages = [
            config.packages.lbf
            config.packages.lbf-pure
            config.packages.lbf-prelude-to-haskell
            config.packages.lbf-plutus-to-haskell
            config.packages.lbf-prelude-to-purescript
            config.packages.lbf-plutus-to-purescript
          ];
        };
      };

      packages = {

        lambda-buffers-frontend-src = pkgs.stdenv.mkDerivation {
          name = "lambda-buffers-frontend-src";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };
        lambda-buffers-frontend-lib = hsNixFlake.packages."lambda-buffers-frontend:lib:lambda-buffers-frontend";
        lambda-buffers-frontend-tests = hsNixFlake.packages."lambda-buffers-frontend:test:tests";
        lambda-buffers-frontend-cli = hsNixFlake.packages."lambda-buffers-frontend:exe:lbf";

        lbf-pure = config.packages.lambda-buffers-frontend-cli;

        lbf = pkgs.writeShellScriptBin "lbf" ''
          export LB_CODEGEN=${config.packages.lbg-haskell}/bin/lbg-haskell;
          export LB_COMPILER=${config.packages.lbc}/bin/lbc;
          ${config.packages.lbf-pure}/bin/lbf "$@"
        '';

        lbf-prelude-to-haskell = pkgs.writeShellScriptBin "lbf-prelude-to-haskell" ''
          export LB_COMPILER=${config.packages.lbc}/bin/lbc;
          mkdir -p autogen;
          mkdir -p .work;
          ${config.overlayAttrs.lbf-nix.lbfBuild.buildCall {
            files = [];
            import-paths = [ config.packages.lbf-prelude ];
            gen = "${config.packages.lbg-haskell}/bin/lbg-haskell";
            gen-classes = ["Prelude.Eq" "Prelude.Json"];
            gen-dir = "autogen";
            gen-opts = ["--config=${config.packages.codegen-configs}/haskell-prelude-base.json"];
            work-dir = ".work";
          }} "$@";
        '';

        lbf-plutus-to-haskell = pkgs.writeShellScriptBin "lbf-plutus-to-haskell" ''
          export LB_COMPILER=${config.packages.lbc}/bin/lbc;
          mkdir -p autogen;
          mkdir -p .work;
          ${config.overlayAttrs.lbf-nix.lbfBuild.buildCall {
            files = [];
            import-paths = [ config.packages.lbf-prelude config.packages.lbf-plutus ];
            gen = "${config.packages.lbg-haskell}/bin/lbg-haskell";
            gen-classes = ["Prelude.Eq" "Prelude.Json" "Plutus.V1.PlutusData" ];
            gen-dir = "autogen";
            gen-opts = [
              "--config=${config.packages.codegen-configs}/haskell-prelude-base.json"
              "--config=${config.packages.codegen-configs}/haskell-plutus-plutustx.json"
            ];
            work-dir = ".work";
          }} "$@";
        '';

        lbf-plutus-to-plutarch = pkgs.writeShellScriptBin "lbf-plutus-to-plutarch" ''
          export LB_COMPILER=${config.packages.lbc}/bin/lbc;
          mkdir autogen;
          mkdir .work;
          ${config.overlayAttrs.lbf-nix.lbfBuild.buildCall {
            files = [];
            import-paths = [ config.packages.lbf-prelude config.packages.lbf-plutus ];
            gen = "${config.packages.lbg-plutarch}/bin/lbg-plutarch";
            gen-classes = ["Prelude.Eq" "Plutus.V1.PlutusData" ];
            gen-dir = "autogen";
            gen-opts = [
              "--config=${config.packages.codegen-configs}/plutarch-prelude.json"
              "--config=${config.packages.codegen-configs}/plutarch-plutus.json"
            ];
            work-dir = ".work";
          }} "$@";
        '';

        lbf-prelude-to-purescript = pkgs.writeShellScriptBin "lbf-prelude-to-purescript" ''
          export LB_COMPILER=${config.packages.lbc}/bin/lbc;
          mkdir -p autogen;
          mkdir -p .work;
          ${config.overlayAttrs.lbf-nix.lbfBuild.buildCall {
            files = [];
            import-paths = [ config.packages.lbf-prelude ];
            gen = "${config.packages.lbg-purescript}/bin/lbg-purescript";
            gen-classes = ["Prelude.Eq" "Prelude.Json"];
            gen-dir = "autogen";
            gen-opts = ["--config=${config.packages.codegen-configs}/purescript-prelude-base.json"];
            work-dir = ".work";
          }} "$@";
        '';

        lbf-plutus-to-purescript = pkgs.writeShellScriptBin "lbf-plutus-to-purescript" ''
          export LB_COMPILER=${config.packages.lbc}/bin/lbc;
          mkdir -p autogen;
          mkdir -p .work;
          ${config.overlayAttrs.lbf-nix.lbfBuild.buildCall {
            files = [];
            import-paths = [ config.packages.lbf-prelude config.packages.lbf-plutus ];
            gen = "${config.packages.lbg-purescript}/bin/lbg-purescript";
            gen-classes = [ "Prelude.Eq" "Prelude.Json" "Plutus.V1.PlutusData" ];
            gen-dir = "autogen";
            gen-opts = [
                "--config=${config.packages.codegen-configs}/purescript-prelude-base.json"
                "--config=${config.packages.codegen-configs}/purescript-plutus-ctl.json"
            ];
            work-dir = ".work";
          }} "$@";
        '';

      };

      inherit (hsNixFlake) checks;

    };
}
