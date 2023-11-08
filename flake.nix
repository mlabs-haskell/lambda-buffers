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
    plutarch = {
      url = "github:plutonomicon/plutarch-plutus";
      flake = false;
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay.follows = "crane/rust-overlay";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./pkgs.nix
        ./settings.nix
        ./pre-commit.nix
        ./hercules-ci.nix
        ./docs/build.nix
        ./docs/plutarch/build.nix
        ./extras/build.nix
        ./extras/lbf-nix/build.nix
        ./libs/build.nix
        ./api/build.nix
        ./lambda-buffers-compiler/build.nix
        ./lambda-buffers-codegen/build.nix
        ./lambda-buffers-frontend/build.nix
        ./runtimes/haskell/lbr-prelude/build.nix
        ./runtimes/haskell/lbr-plutus/build.nix
        ./runtimes/haskell/lbr-plutarch/build.nix
        ./runtimes/purescript/lbr-prelude/build.nix
        ./runtimes/purescript/lbr-plutus/build.nix
        ./runtimes/rust/lbr-prelude/build.nix
        ./testsuites/lbt-prelude/api/build.nix
        ./testsuites/lbt-prelude/golden/build.nix
        ./testsuites/lbt-prelude/lbt-prelude-haskell/build.nix
        ./testsuites/lbt-prelude/lbt-prelude-purescript/build.nix
        ./testsuites/lbt-plutus/api/build.nix
        ./testsuites/lbt-plutus/golden/build.nix
        ./testsuites/lbt-plutus/lbt-plutus-haskell/build.nix
        ./testsuites/lbt-plutus/lbt-plutus-purescript/build.nix
        ./testsuites/lbt-plutus/lbt-plutus-plutarch/build.nix
        ./experimental/build.nix
      ];
      debug = true;
      systems = [ "x86_64-linux" "x86_64-darwin" ];
    };
}
