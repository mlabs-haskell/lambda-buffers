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
        (import ./hercules-ci.nix)
        (import ./pre-commit.nix)
        (import ./docs/build.nix)
      ];
      debug = true;
      systems = [ "x86_64-linux" "x86_64-darwin" ];
      perSystem = { system, config, ... }:
        let
          inherit self;

          # Nixpkgs with Haskell.nix

          pkgs = import nixpkgs {
            inherit system;
            inherit (inputs.haskell-nix) config;
            overlays = [
              inputs.haskell-nix.overlay
              inputs.iohk-nix.overlays.crypto
              inputs.ctl.overlays.purescript
              inputs.ctl.overlays.spago
            ];
          };
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

          # Nix libs

          lbfHaskell = import ./extras/lbf-haskell.nix clis.lbf clis.lbg-haskell;
          lbfPurescript = import ./extras/lbf-purescript.nix clis.lbf clis.lbg-purescript;
          lbfHaskellPlutus = import ./extras/lbf-haskell-plutus.nix clis.lbf clis.lbg-haskell;
          lbfPurescriptPlutus = import ./extras/lbf-purescript-plutus.nix clis.lbf clis.lbg-purescript;
          pursFlake = import ./extras/flake-purescript.nix;

          # Runtimes

          ## Prelude runtime - lbr-prelude

          ### Haskell

          lbrPreludeHsBuild = buildAbstraction {
            import-location = ./runtimes/haskell/lbr-prelude/build.nix;
            additional = { };
          };
          lbrPreludeHsFlake = flakeAbstraction lbrPreludeHsBuild;

          ### Purescript

          lbrPreludePurs = pursFlake (
            import ./runtimes/purescript/lbr-prelude/build.nix {
              inherit pkgs commonTools;
              shellHook = config.pre-commit.installationScript;
            }
          );

          ## Plutus runtime - lbr-plutus

          ### Haskell
          lbrPlutusHsBuild = buildAbstraction {
            import-location = ./runtimes/haskell/lbr-plutus/build.nix;
            additional = { lbr-prelude = ./runtimes/haskell/lbr-prelude; };
          };
          lbrPlutusHsFlake = flakeAbstraction lbrPlutusHsBuild;

          ### Purescript

          lbrPlutusPurs = pursFlake (
            import ./runtimes/purescript/lbr-plutus/build.nix {
              inherit pkgs commonTools;
              shellHook = config.pre-commit.installationScript;
            }
          );

          # Schema libs

          lbfLibs = {
            lbf-prelude-hs = lbfHaskell {
              inherit pkgs;
              name = "lbf-prelude";
              src = ./libs/lbf-prelude;
              files = [ "Prelude.lbf" ];
              dependencies = [ "lbr-prelude" ];
            };

            lbf-prelude-purs = lbfPurescript {
              inherit pkgs;
              name = "lbf-prelude";
              src = ./libs/lbf-prelude;
              files = [ "Prelude.lbf" ];
              dependencies = [ "lbr-prelude" ];
            };

            lbf-plutus-hs = lbfHaskell {
              inherit pkgs;
              name = "lbf-plutus";
              src = ./libs/lbf-plutus;
              imports = [ ./libs/lbf-prelude ];
              files = [ "Plutus/V1.lbf" "Plutus/V2.lbf" ];
              dependencies = [ "lbr-plutus" "lbf-prelude" "lbr-prelude" ];
            };

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

          ### Haskell

          lbtPreludeHsBuild = buildAbstraction {
            import-location = ./testsuites/lbt-prelude/lbt-prelude-haskell/build.nix;
            additional = {
              inherit lbfHaskell;
              lbf-prelude = ./libs/lbf-prelude;
              lbr-prelude-hs = ./runtimes/haskell/lbr-prelude;
              inherit (lbfLibs) lbf-prelude-hs;
            };
          };
          lbtPreludeHsFlake = flakeAbstraction lbtPreludeHsBuild;

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
          lbtPreludePursFlake = pursFlake (
            import ./testsuites/lbt-prelude/lbt-prelude-purescript/build.nix {
              inherit pkgs commonTools shellHook lbfPurescript;
              inherit (lbrPurs) lbr-prelude-purs;
              inherit (lbfLibs) lbf-prelude-purs;
            }
          );

          ## Plutus test suite - lbt-plutus

          ### Haskell

          lbtPlutusHsBuild = buildAbstraction {
            import-location = ./testsuites/lbt-plutus/lbt-plutus-haskell/build.nix;
            additional = {
              inherit lbfHaskellPlutus;
              lbf-prelude = ./libs/lbf-prelude;
              lbr-prelude-hs = ./runtimes/haskell/lbr-prelude;
              lbf-plutus = ./libs/lbf-plutus;
              lbr-plutus-hs = ./runtimes/haskell/lbr-plutus;
              inherit (lbfLibs) lbf-prelude-hs lbf-plutus-hs;
            };
          };
          lbtPlutusHsFlake = flakeAbstraction lbtPlutusHsBuild;

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
            // lbrPreludeHsFlake.packages
            // lbrPreludePurs.packages
            // lbrPlutusHsFlake.packages
            // lbrPlutusPurs.packages
            // lbtPreludeHsFlake.packages
            // lbtPreludePursFlake.packages
            // lbtPlutusHsFlake.packages
            // lbtPlutusPursFlake.packages
            // clis
            // lbfLibs;

          devShells = rec {
            dev-experimental = experimentalDevShell;
            dev-protos = protosBuild.devShell;
            dev-compiler = compilerFlake.devShell;
            dev-frontend = frontendFlake.devShell;
            dev-codegen = codegenFlake.devShell;
            dev-lbr-prelude-haskell = lbrPreludeHsFlake.devShell;
            dev-lbr-prelude-purescript = lbrPreludePurs.devShell;
            dev-lbr-plutus-haskell = lbrPlutusHsFlake.devShell;
            dev-lbr-plutus-purescript = lbrPlutusPurs.devShell;
            dev-lbt-prelude-haskell = lbtPreludeHsFlake.devShell;
            dev-lbt-prelude-purescript = lbtPreludePursFlake.devShell;
            dev-lbt-plutus-haskell = lbtPlutusHsFlake.devShell;
            dev-lbt-plutus-purescript = lbtPlutusPursFlake.devShell;
            lb = lbEnv;
          };

          # nix flake check
          checks = devShells //
            packages //
            lbrPreludePurs.checks //
            lbrPlutusPurs.checks //
            lbtPreludePursFlake.checks //
            lbtPlutusPursFlake.checks //
            renameAttrs (n: "check-${n}") (
              compilerFlake.checks //
                frontendFlake.checks //
                codegenFlake.checks //
                lbrPreludeHsFlake.checks //
                lbrPlutusHsFlake.checks //
                lbtPreludeHsFlake.checks //
                lbtPlutusHsFlake.checks
            );

        };
    };
}
