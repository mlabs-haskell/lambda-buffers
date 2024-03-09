{ inputs, ... }:
{
  perSystem = { config, pkgs, system, ... }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellFlake {
        src = ./.;

        name = "lambda-buffers-frontend";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          "${config.packages.lambda-buffers-lang-hs-pb}"
          "${config.packages.lambda-buffers-compiler-hs-pb}"
          "${config.packages.lambda-buffers-codegen-hs-pb}"
          "${config.packages.lambda-buffers-compiler-src}"
        ];

        devShellTools = [
          config.packages.lbc
          config.packages.lbg
          config.packages.lbg-haskell
          config.packages.lbg-purescript
          config.packages.lbg-rust
        ] ++ config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in

    {
      devShells = {
        dev-frontend = hsFlake.devShell;

        lb = pkgs.mkShell {
          name = "lambdabuffers-env";
          packages = [
            config.packages.lbf
            config.packages.lbf-pure
            config.packages.lbf-prelude-to-haskell
            config.packages.lbf-plutus-to-haskell
            config.packages.lbf-prelude-to-purescript
            config.packages.lbf-plutus-to-purescript
            config.packages.lbf-prelude-to-rust
            config.packages.lbf-plutus-to-rust
            config.packages.lbf-list-modules-typescript
            config.packages.lbf-prelude-to-typescript
            config.packages.lbf-plutus-to-typescript
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
        lambda-buffers-frontend-lib = hsFlake.packages."lambda-buffers-frontend:lib:lambda-buffers-frontend";
        lambda-buffers-frontend-tests = hsFlake.packages."lambda-buffers-frontend:test:tests";
        lambda-buffers-frontend-cli = hsFlake.packages."lambda-buffers-frontend:exe:lbf";

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
          ${config.lbf-nix.lbfBuild.buildCall {
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
          ${config.lbf-nix.lbfBuild.buildCall {
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
          ${config.lbf-nix.lbfBuild.buildCall {
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
          ${config.lbf-nix.lbfBuild.buildCall {
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
          ${config.lbf-nix.lbfBuild.buildCall {
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


        lbf-prelude-to-typescript = pkgs.writeShellScriptBin "lbf-prelude-to-typescript" ''
          export LB_COMPILER=${config.packages.lbc}/bin/lbc;
          mkdir -p autogen;
          mkdir -p .work;
          ${config.lbf-nix.lbfBuild.buildCall {
            files = [];
            import-paths = [ config.packages.lbf-prelude ];
            gen = "${config.packages.lbg-typescript}/bin/lbg-typescript";
            gen-classes = ["Prelude.Eq" "Prelude.Json"];
            gen-dir = "autogen";
            gen-opts = ["--config=${config.packages.codegen-configs}/typescript-prelude-base.json"];
            work-dir = ".work";
          }} "$@";
        '';

        lbf-plutus-to-typescript = pkgs.writeShellScriptBin "lbf-plutus-to-typescript" ''
          export LB_COMPILER=${config.packages.lbc}/bin/lbc;
          mkdir -p autogen;
          mkdir -p .work;
          ${config.lbf-nix.lbfBuild.buildCall {
            files = [];
            import-paths = [ config.packages.lbf-prelude config.packages.lbf-plutus ];
            gen = "${config.packages.lbg-typescript}/bin/lbg-typescript";
            gen-classes = [ "Prelude.Eq" "Prelude.Json" "Plutus.V1.PlutusData" ];
            gen-dir = "autogen";
            gen-opts = [
              "--config=${config.packages.codegen-configs}/typescript-prelude-base.json"
              "--config=${config.packages.codegen-configs}/typescript-plutus.json"
            ];
            work-dir = ".work";
          }} "$@";
        '';

        lbf-prelude-to-rust = pkgs.writeShellScriptBin "lbf-prelude-to-rust" ''
          export LB_COMPILER=${config.packages.lbc}/bin/lbc;
          mkdir -p autogen;
          mkdir -p .work;
          ${config.lbf-nix.lbfBuild.buildCall {
            files = [];
            import-paths = [ config.packages.lbf-prelude ];
            gen = "${config.packages.lbg-rust}/bin/lbg-rust";
            gen-classes = ["Prelude.Eq" "Prelude.Json"];
            gen-dir = "autogen";
            gen-opts = [
              "--packages lb-pkgs.json"
              "--config=${config.packages.codegen-configs}/rust-prelude-base.json"];
            work-dir = ".work";
          }} "$@";
        '';

        lbf-plutus-to-rust = pkgs.writeShellScriptBin "lbf-plutus-to-rust" ''
          export LB_COMPILER=${config.packages.lbc}/bin/lbc;
          mkdir -p autogen;
          mkdir -p .work;
          ${config.lbf-nix.lbfBuild.buildCall {
            files = [];
            import-paths = [ config.packages.lbf-prelude config.packages.lbf-plutus ];
            gen = "${config.packages.lbg-rust}/bin/lbg-rust";
            gen-classes = ["Prelude.Eq" "Prelude.Json" "Plutus.V1.PlutusData" ];
            gen-dir = "autogen";
            gen-opts = [
              "--packages lb-pkgs.json"
              "--config=${config.packages.codegen-configs}/rust-prelude-base.json"
              "--config=${config.packages.codegen-configs}/rust-plutus-pla.json"
            ];
            work-dir = ".work";
          }} "$@";
        '';

      };


      inherit (hsFlake) checks;

    };
}
