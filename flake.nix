{
  description = "Lambda Buffers";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    # flake-lang.nix used for monorepo setups
    flake-lang.url = "github:mlabs-haskell/flake-lang.nix";

    # Flakes as modules, using this extensively to organize the repo into modules (build.nix files)
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Code quality automation
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    # Hercules CI effects
    hci-effects.url = "github:hercules-ci/hercules-ci-effects";

    # Nix library for Google Protobufs
    proto-nix.url = "github:mlabs-haskell/proto.nix";

    # Cardano transaction library (leveraging CTL's Purescript Nix machinery)
    ctl.url = "github:plutonomicon/cardano-transaction-lib?ref=develop";

    # TODO(bladyjoker): Remove once flake-lang.nix fixes this
    crane.url = "github:ipetkov/crane";

    # Plutarch eDSL (LB Codegen target)
    plutarch.url = "github:Plutonomicon/plutarch-plutus";

    # Typescript runtimes
    prelude-typescript.follows = "plutus-ledger-api-typescript/prelude-typescript";
    plutus-ledger-api-typescript = {
      url = "github:mlabs-haskell/plutus-ledger-api-typescript?ref=jared/add-follows-to-flake-nix";
      inputs.flake-lang.follows = "flake-lang";
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
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
        ./runtimes/rust/lbr-prelude-derive/build.nix
        ./runtimes/typescript/lbr-prelude/build.nix
        ./runtimes/typescript/lbr-plutus/build.nix
        ./testsuites/lbt-prelude/api/build.nix
        ./testsuites/lbt-prelude/golden/build.nix
        ./testsuites/lbt-prelude/lbt-prelude-haskell/build.nix
        ./testsuites/lbt-prelude/lbt-prelude-purescript/build.nix
        ./testsuites/lbt-prelude/lbt-prelude-rust/build.nix
        ./testsuites/lbt-prelude/lbt-prelude-typescript/build.nix
        ./testsuites/lbt-plutus/api/build.nix
        ./testsuites/lbt-plutus/golden/build.nix
        ./testsuites/lbt-plutus/lbt-plutus-haskell/build.nix
        ./testsuites/lbt-plutus/lbt-plutus-purescript/build.nix
        ./testsuites/lbt-plutus/lbt-plutus-typescript/build.nix
        ./testsuites/lbt-plutus/lbt-plutus-plutarch/build.nix
        ./testsuites/lbt-plutus/lbt-plutus-rust/build.nix
        ./experimental/build.nix
        ./docs/typescript-prelude/build.nix
        ./docs/typescript-plutus/build.nix
      ];
      debug = true;
      systems = [ "x86_64-linux" "x86_64-darwin" ];
    };
}
