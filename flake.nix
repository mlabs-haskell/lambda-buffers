{
  description = "Lambda Buffers";
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    protobufs-nix.url = "github:mlabs-haskell/protobufs.nix";
    mlabs-tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
    hci-effects.url = "github:hercules-ci/hercules-ci-effects";
    ctl.url = "github:plutonomicon/cardano-transaction-lib/bladyjoker/lambda-buffers-catalyst"; # path:/home/bladyjoker/Desktop/cardano-transaction-lib;
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    flake-parts.url = "github:hercules-ci/flake-parts";
    purifix.url = "github:purifix/purifix";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, pre-commit-hooks, protobufs-nix, mlabs-tooling, hci-effects, iohk-nix, flake-parts, purifix, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        (import ./pkgs.nix)
        (import ./settings.nix)
        (import ./hercules-ci.nix)
        (import ./pre-commit.nix)
        (import ./docs/build.nix)
        (import ./extras/build.nix)
        (import ./extras/lbf-nix/build.nix)
        (import ./libs/build.nix)
        (import ./runtimes/haskell/lbr-prelude/build.nix)
        (import ./runtimes/haskell/lbr-plutus/build.nix)
        (import ./runtimes/purescript/lbr-prelude/build.nix)
        (import ./testsuites/lbt-prelude/api/build.nix)
        (import ./testsuites/lbt-prelude/golden/build.nix)
        (import ./testsuites/lbt-prelude/lbt-prelude-haskell/build.nix)
        (import ./testsuites/lbt-prelude/lbt-prelude-purescript/build.nix)
        (import ./testsuites/lbt-plutus/api/build.nix)
        (import ./testsuites/lbt-plutus/golden/build.nix)
        (import ./testsuites/lbt-plutus/lbt-plutus-haskell/build.nix)
      ];
      debug = true;
      systems = [ "x86_64-linux" "x86_64-darwin" ];
      perSystem = { system, config, pkgs, ... }:
        let
          inherit self;

          haskell-nix = pkgs.haskell-nix;

          # pre-commit-hooks.nix

          fourmolu = pkgs.haskell.packages.ghc924.fourmolu;

          apply-refact = pkgs.haskellPackages.apply-refact;

          commonTools = {
            inherit (pre-commit-hooks.outputs.packages.${system}) nixpkgs-fmt cabal-fmt shellcheck hlint typos markdownlint-cli dhall purty;
            inherit (pkgs) protolint txtpbfmt;
            inherit fourmolu;
            inherit apply-refact;
          };

          shellHook = config.pre-commit.installationScript;

          # Experimental env

          experimentalDevShell = import ./experimental/build.nix {
            inherit pkgs commonTools shellHook;
          };

          # Protos build

          pbnix-lib = protobufs-nix.lib.${system};

          protosBuild = import ./api/build.nix {
            inherit pkgs pbnix-lib commonTools shellHook;
          };

          index-state = "2022-12-01T00:00:00Z";
          compiler-nix-name = "ghc925";

          # Common build abstraction for the components.

          buildAbstraction = { import-location, additional ? { } }:
            import import-location ({
              inherit pkgs compiler-nix-name index-state haskell-nix mlabs-tooling commonTools shellHook;
            } // additional);

          # Common Flake abstraction for the components.

          flakeAbstraction = component-name: component-name.hsNixProj.flake { };

          # Compiler Build

          compilerBuild = buildAbstraction {
            import-location = ./lambda-buffers-compiler/build.nix;
            additional = { inherit (protosBuild.packages) lambda-buffers-lang-hs-pb lambda-buffers-compiler-hs-pb lambda-buffers-codegen-hs-pb; };
          };
          compilerFlake = flakeAbstraction compilerBuild;

          # Codegen Build

          codegenBuild = buildAbstraction {
            import-location = ./lambda-buffers-codegen/build.nix;
            additional = {
              inherit (protosBuild.packages) lambda-buffers-lang-hs-pb lambda-buffers-compiler-hs-pb lambda-buffers-codegen-hs-pb;
              lambda-buffers-compiler = ./lambda-buffers-compiler;
            };
          };
          codegenFlake = flakeAbstraction codegenBuild;

          # Frontend Build

          frontendBuild = buildAbstraction {
            import-location = ./lambda-buffers-frontend/build.nix;
            additional = {
              inherit (protosBuild.packages) lambda-buffers-lang-hs-pb lambda-buffers-compiler-hs-pb lambda-buffers-codegen-hs-pb;
              lambda-buffers-compiler = ./lambda-buffers-compiler;
              inherit (clis) lbc lbg lbg-haskell lbg-purescript;
            };
          };
          frontendFlake = flakeAbstraction frontendBuild;

          # LambdaBuffers CLIs

          clis = rec {
            lbf-pure = frontendFlake.packages."lambda-buffers-frontend:exe:lbf";
            lbc = compilerFlake.packages."lambda-buffers-compiler:exe:lbc";
            lbg = codegenFlake.packages."lambda-buffers-codegen:exe:lbg";
            lbg-haskell = pkgs.writeShellScriptBin "lbg-haskell" ''
              ${lbg}/bin/lbg gen-haskell $@
            '';
            lbg-purescript = pkgs.writeShellScriptBin "lbg-purescript" ''
              ${lbg}/bin/lbg gen-purescript $@
            '';
            lbf = pkgs.writeShellScriptBin "lbf" ''
              export LB_CODEGEN=${lbg-haskell}/bin/lbg-haskell;
              export LB_COMPILER=${lbc}/bin/lbc;
              ${lbf-pure}/bin/lbf $@
            '';
            lbf-to-haskell = pkgs.writeShellScriptBin "lbf-to-haskell" ''
              export LB_COMPILER=${lbc}/bin/lbc;

              ${lbf-pure}/bin/lbf build --gen ${lbg-haskell}/bin/lbg-haskell $@
            '';
            lbf-to-haskell-prelude = pkgs.writeShellScriptBin "lbf-to-haskell-prelude" ''
              export LB_COMPILER=${lbc}/bin/lbc;

              ${lbf-pure}/bin/lbf build --import-path ${./libs/lbf-prelude} \
                  --gen-class Prelude.Eq --gen-class Prelude.Json \
                  --gen ${lbg-haskell}/bin/lbg-haskell $@
            '';
            lbf-to-purescript = pkgs.writeShellScriptBin "lbf-to-purescript" ''
              export LB_COMPILER=${lbc}/bin/lbc;

              ${lbf-pure}/bin/lbf build --gen ${lbg-purescript}/bin/lbg-purescript $@
            '';
            lbf-to-purescript-prelude = pkgs.writeShellScriptBin "lbf-to-purescript-prelude" ''
              export LB_COMPILER=${lbc}/bin/lbc;

              ${lbf-pure}/bin/lbf build --import-path ${./libs/lbf-prelude} \
                  --gen-class Prelude.Eq --gen-class Prelude.Json \
                  --gen ${lbg-purescript}/bin/lbg-purescript $@
            '';

          };

          # LambdaBuffers environment

          lbEnv = pkgs.mkShell {
            name = "lambdabuffers-env";
            packages = builtins.attrValues clis;
          };

          # lbf Nix API

          lbfPurescript = import ./extras/lbf-nix/lbf-purescript.nix clis.lbf clis.lbg-purescript;
          lbfPurescriptPlutus = import ./extras/lbf-nix/lbf-purescript-plutus.nix pkgs clis.lbf clis.lbg-purescript;
          pursFlake = import ./extras/flake-purescript.nix;

          # Runtimes

          ## Plutus runtime - lbr-plutus

          ### Purescript

          lbrPlutusPurs = pursFlake (
            import ./runtimes/purescript/lbr-plutus/build.nix {
              inherit pkgs commonTools;
              shellHook = config.pre-commit.installationScript;
            }
          );

          # Schema libs

          lbfLibs = {

            lbf-plutus-purs = lbfPurescript {
              inherit pkgs;
              name = "lbf-plutus";
              src = ./libs/lbf-plutus;
              imports = [ ./libs/lbf-prelude ];
              files = [ "Plutus/V1.lbf" "Plutus/V2.lbf" ];
              dependencies = [ "lbr-plutus" "lbf-prelude" "lbr-prelude" ];
            };
          };

          # Test Suites

          ## Prelude test suite - lbt-prelude

          ### Purescript

          lbrPurs = {
            lbr-prelude-purs = pkgs.stdenv.mkDerivation {
              name = "lbr-prelude";
              src = ./runtimes/purescript/lbr-prelude;
              phases = "installPhase";
              installPhase = "ln -s $src $out";
            };
            lbr-plutus-purs = pkgs.stdenv.mkDerivation {
              name = "lbr-plutus";
              src = ./runtimes/purescript/lbr-plutus;
              phases = "installPhase";
              installPhase = "ln -s $src $out";
            };

          };

          ## Plutus test suite - lbt-plutus

          ### Purescript

          lbtPlutusPursFlake = pursFlake (
            import ./testsuites/lbt-plutus/lbt-plutus-purescript/build.nix {
              inherit pkgs commonTools shellHook lbfPurescriptPlutus;
              inherit (lbrPurs) lbr-prelude-purs lbr-plutus-purs;
              inherit (lbfLibs) lbf-prelude-purs lbf-plutus-purs;
            }
          );

          # Utilities
          renameAttrs = rnFn: pkgs.lib.attrsets.mapAttrs' (n: value: { name = rnFn n; inherit value; });
        in
        rec
        {
          # Standard flake attributes
          packages = protosBuild.packages
            // compilerFlake.packages
            // frontendFlake.packages
            // codegenFlake.packages
            // clis
            // lbfLibs;

          devShells = rec {
            dev-experimental = experimentalDevShell;
            dev-protos = protosBuild.devShell;
            dev-compiler = compilerFlake.devShell;
            dev-frontend = frontendFlake.devShell;
            dev-codegen = codegenFlake.devShell;
            lb = lbEnv;
          };

          # nix flake check
          checks = devShells //
            packages //
            renameAttrs (n: "check-${n}") (
              compilerFlake.checks //
                frontendFlake.checks //
                codegenFlake.checks
            );

        };
    };
}
