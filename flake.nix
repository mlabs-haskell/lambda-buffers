{
  description = "Lambda Buffers";
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    protobufs-nix.url = "github:mlabs-haskell/protobufs.nix";
    mlabs-tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, pre-commit-hooks, protobufs-nix, mlabs-tooling, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
        let
          inherit self;

          # Nixpkgs with Haskell.nix
          pkgs = import nixpkgs {
            inherit system;
            inherit (inputs.haskell-nix) config;
            overlays = [ inputs.haskell-nix.overlay ];
          };
          haskell-nix = pkgs.haskell-nix;

          # pre-commit-hooks.nix
          fourmolu = pkgs.haskell.packages.ghc924.fourmolu;

          # pre-commit-hooks.nix
          apply-refact = pkgs.haskellPackages.apply-refact;

          pre-commit-check = pre-commit-hooks.lib.${system}.run (import ./pre-commit-check.nix {
            inherit fourmolu;
            protoHooks = pbnix-lib.preCommitHooks { inherit pkgs; };
          });

          commonTools = {
            inherit (pre-commit-hooks.outputs.packages.${system}) nixpkgs-fmt nix-linter cabal-fmt shellcheck hlint typos markdownlint-cli dhall;
            inherit (pkgs) protolint txtpbfmt;
            inherit fourmolu;
            inherit apply-refact;
          };

          preCommitDevShell = pkgs.mkShell {
            name = "pre-commit-env";
            inherit (pre-commit-check) shellHook;
          };

          # Experimental env
          experimentalDevShell = import ./experimental/build.nix {
            inherit pkgs commonTools;
            inherit (pre-commit-check) shellHook;
          };

          # Docs env
          docsDevShell = import ./docs/build.nix {
            inherit pkgs commonTools;
            inherit (pre-commit-check) shellHook;
          };

          # Protos build
          pbnix-lib = protobufs-nix.lib.${system};

          protosBuild = import ./lambda-buffers-proto/build.nix {
            inherit pkgs pbnix-lib commonTools;
            inherit (pre-commit-check) shellHook;
          };

          index-state = "2022-12-01T00:00:00Z";
          compiler-nix-name = "ghc924";

          # Common build abstraction for the components.
          buildAbstraction = { import-location, additional ? { } }:
            import import-location ({
              inherit pkgs compiler-nix-name index-state haskell-nix mlabs-tooling commonTools;
              inherit (protosBuild) compilerHsPb;
              inherit (pre-commit-check) shellHook;
            } // additional);

          # Common Flake abstraction for the components.
          flakeAbstraction = component-name: component-name.hsNixProj.flake { };

          # Compiler Build
          compilerBuild = buildAbstraction { import-location = ./lambda-buffers-compiler/build.nix; };
          compilerFlake = flakeAbstraction compilerBuild;

          # Extras Build
          extrasBuild = buildAbstraction {
            import-location = ./lambda-buffers-extras/build.nix;
            additional = { lambda-buffers-compiler = ./lambda-buffers-compiler; };
          };
          extrasFlake = flakeAbstraction extrasBuild;

          # Frontend Build
          frontendBuild = buildAbstraction {
            import-location = ./lambda-buffers-frontend/build.nix;
            additional = { lambda-buffers-compiler = ./lambda-buffers-compiler; };
          };
          frontendFlake = flakeAbstraction frontendBuild;

          # Utilities
          renameAttrs = rnFn: pkgs.lib.attrsets.mapAttrs' (n: value: { name = rnFn n; inherit value; });
        in
        rec {
          # Useful for nix repl
          inherit pkgs;

          # Standard flake attributes
          packages = { inherit (protosBuild) compilerHsPb; } // compilerFlake.packages // frontendFlake.packages // extrasFlake.packages;

          devShells = rec {
            dev-pre-commit = preCommitDevShell;
            dev-experimental = experimentalDevShell;
            dev-docs = docsDevShell;
            dev-protos = protosBuild.devShell;
            dev-compiler = compilerFlake.devShell;
            dev-frontend = frontendFlake.devShell;
            dev-common = extrasFlake.devShell;
            default = preCommitDevShell;
          };

          # nix flake check --impure --keep-going --allow-import-from-derivation
          checks = { inherit pre-commit-check; } // devShells // packages // renameAttrs (n: "check-${n}") (frontendFlake.checks // compilerFlake.checks // extrasFlake.checks);

        }
      ) // {
      # Instruction for the Hercules CI to build on x86_64-linux only, to avoid errors about systems without agents.
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
