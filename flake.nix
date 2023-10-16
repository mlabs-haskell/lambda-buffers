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

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        (import ./pkgs.nix)
        (import ./settings.nix)
        (import ./pre-commit.nix)
        (import ./hercules-ci.nix)
        (import ./docs/build.nix)
        (import ./extras/build.nix)
        (import ./extras/lbf-nix/build.nix)
        (import ./libs/build.nix)
        (import ./api/build.nix)
        (import ./lambda-buffers-compiler/build.nix)
        (import ./lambda-buffers-codegen/build.nix)
        (import ./lambda-buffers-frontend/build.nix)
        (import ./runtimes/haskell/lbr-prelude/build.nix)
        (import ./runtimes/haskell/lbr-plutus/build.nix)
        (import ./runtimes/purescript/lbr-prelude/build.nix)
        (import ./runtimes/purescript/lbr-plutus/build.nix)
        (import ./testsuites/lbt-prelude/api/build.nix)
        (import ./testsuites/lbt-prelude/golden/build.nix)
        (import ./testsuites/lbt-prelude/lbt-prelude-haskell/build.nix)
        (import ./testsuites/lbt-prelude/lbt-prelude-purescript/build.nix)
        (import ./testsuites/lbt-plutus/api/build.nix)
        (import ./testsuites/lbt-plutus/golden/build.nix)
        (import ./testsuites/lbt-plutus/lbt-plutus-haskell/build.nix)
        (import ./testsuites/lbt-plutus/lbt-plutus-purescript/build.nix)
        (import ./experimental/build.nix)
      ];
      debug = true;
      systems = [ "x86_64-linux" "x86_64-darwin" ];
    };
}
