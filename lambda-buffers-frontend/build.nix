self@{ inputs, ... }:
{
  perSystem = { pkgs, system, inputs', config, ... }:
    let
      project = { lib, ... }: {
        src = ./.;

        name = "lambda-buffers-frontend";

        inherit (self.config.settings.haskell) index-state compiler-nix-name;

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
          ]; # ++ builtins.attrValues commonTools;

          tools = {
            cabal = { };
            haskell-language-server = { };
          };

          shellHook = lib.mkForce ''
            export LC_CTYPE=C.UTF-8;
            export LC_ALL=C.UTF-8;
            export LANG=C.UTF-8;
            ${config.pre-commit.installationScript}
          '';
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
          ${config.packages.lbf-pure}/bin/lbf $@
        '';

        lbf-prelude-to-haskell = pkgs.writeShellScriptBin "lbf-prelude-to-haskell" ''
          export LB_COMPILER=${config.packages.lbc}/bin/lbc;
          mkdir autogen;
          mkdir .work;
          ${config.overlayAttrs.lbf-nix.lbfBuild.buildCall {
            files = [];
            import-paths = [ config.packages.lbf-prelude ];
            gen = "${config.packages.lbg-haskell}/bin/lbg-haskell";
            gen-classes = ["Prelude.Eq" "Prelude.Json"];
            gen-dir = "autogen";
            gen-opts = ["--config=${config.packages.codegen-configs}/haskell-prelude-base.json"];
            work-dir = ".work";
          }} $@;
        '';

        lbf-plutus-to-haskell = pkgs.writeShellScriptBin "lbf-plutus-to-haskell" ''
          export LB_COMPILER=${config.packages.lbc}/bin/lbc;
          mkdir autogen;
          mkdir .work;
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
          }} $@;
        '';
      };

      inherit (hsNixFlake) checks;

    };
}
