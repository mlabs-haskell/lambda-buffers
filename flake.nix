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
    ctl.url = "github:Plutonomicon/cardano-transaction-lib/v5.0.0";
    iohk-nix = { url = "github:input-output-hk/iohk-nix"; flake = false; };
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, pre-commit-hooks, protobufs-nix, mlabs-tooling, hci-effects, iohk-nix, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./hercules-ci.nix
        ./pre-commit.nix
        ./common.nix
        ./lambda-buffers-proto/build.nix
        ./lambda-buffers-compiler/build.nix
      ];
      systems = [ "x86_64-linux" ];
      debug = true;
      #      perSystem = { system, config, ... }:
      # let
      #   # Nixpkgs with Haskell.nix
      #   pkgs = import nixpkgs {
      #     inherit system;
      #     inherit (inputs.haskell-nix) config;
      #     overlays = [
      #       inputs.haskell-nix.overlay
      #       (import "${iohk-nix}/overlays/crypto")
      #     ];
      #   };
      #   haskell-nix = pkgs.haskell-nix;

      #   # Experimental env
      #   experimentalDevShell = import ./experimental/build.nix {
      #     inherit pkgs;
      #     commonTools = config.common.tools;
      #     shellHook = config.common.shellHook;
      #   };

      #   # Docs env
      #   docsDevShell = import ./docs/build.nix {
      #     inherit pkgs;
      #     commonTools = config.common.tools;
      #     shellHook = config.common.shellHook;
      #   };

      #   # Common build abstraction for the components.
      #   buildAbstraction = { import-location, additional ? { } }:
      #     import import-location ({
      #       inherit pkgs haskell-nix mlabs-tooling;
      #       inherit (config.packages) compilerHsPb;
      #       commonTools = config.common.tools;
      #       inherit (config.common) shellHook compiler-nix-name index-state;
      #     } // additional);

      #   # Common Flake abstraction for the components.
      #   flakeAbstraction = component-name: component-name.hsNixProj.flake { };

      #   # Codegen Build
      #   codegenBuild = buildAbstraction {
      #     import-location = ./lambda-buffers-codegen/build.nix;
      #     additional = {
      #       lambda-buffers-compiler = ./lambda-buffers-compiler;
      #     };
      #   };
      #   codegenFlake = flakeAbstraction codegenBuild;

      #   # Frontend Build
      #   frontendBuild = buildAbstraction {
      #     import-location = ./lambda-buffers-frontend/build.nix;
      #     additional = {
      #       lambda-buffers-compiler = ./lambda-buffers-compiler;
      #       lbc = pkgs.hello; #config.packages."lambda-buffers-compiler:exe:lbc";
      #       lbg = codegenFlake.packages."lambda-buffers-codegen:exe:lbg";
      #     };
      #   };
      #   frontendFlake = flakeAbstraction frontendBuild;

      #   # LambdaBuffers CLIs
      #   clis = {
      #     lbf = frontendFlake.packages."lambda-buffers-frontend:exe:lbf";
      #     lbc = pkgs.hello; #config.packages."lambda-buffers-compiler:exe:lbc";
      #     lbg = codegenFlake.packages."lambda-buffers-codegen:exe:lbg";
      #   };

      #   # Purescript/cardano-transaction-lib environment.
      #   ctlShell = import ./experimental/ctl-env/build.nix {
      #     inherit system; inherit (inputs) nixpkgs ctl;
      #     inherit (clis) lbf lbc lbg;
      #     lbf-base = ./experimental/lbf-base;
      #   };
      #   # Purescript/cardano-transaction-lib shell
      #   plutusTxShell = import ./experimental/plutustx-env/build.nix {
      #     inherit pkgs haskell-nix mlabs-tooling;
      #     inherit (config.common) compiler-nix-name index-state;
      #     inherit (clis) lbf lbc lbg;
      #     lbf-base = ./experimental/lbf-base;
      #   };

      #   # Utilities
      #   renameAttrs = rnFn: pkgs.lib.attrsets.mapAttrs' (n: value: { name = rnFn n; inherit value; });
      # in
      # rec {
      #   # Standard flake attributes
      #   # packages = frontendFlake.packages // codegenFlake.packages;

      #   # devShells = {
      #   #   dev-experimental = experimentalDevShell;
      #   #   dev-docs = docsDevShell;
      #   #   dev-frontend = frontendFlake.devShell;
      #   #   dev-codegen = codegenFlake.devShell;
      #   #   dev-ctl-env = ctlShell;
      #   #   dev-plutustx-env = plutusTxShell;
      #   # };

      #   # # nix flake check
      #   # checks = renameAttrs (n: "check-${n}") (frontendFlake.checks // codegenFlake.checks);

      # };
    };
}
