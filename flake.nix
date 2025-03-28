{
  description = "Lambda Buffers";
  inputs = {
    # flake-lang.nix used for monorepo setups
    flake-lang.url = "github:mlabs-haskell/flake-lang.nix";
    flake-lang.inputs.ctl.follows = "ctl";

    nixpkgs.follows = "flake-lang/nixpkgs";

    # Flakes as modules, using this extensively to organize the repo into modules (build.nix files)
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Code quality automation
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    # Hercules CI effects
    hci-effects.url = "github:hercules-ci/hercules-ci-effects";

    # Nix library for Google Protobufs
    proto-nix.url = "github:mlabs-haskell/proto.nix";

    # Cardano transaction library (leveraging CTL's Purescript Nix machinery)
    ctl.url = "github:Plutonomicon/cardano-transaction-lib/5b0a18b5a79c1ee024ca2668af04fab42c444e8f";

    # Plutarch eDSL (LB Codegen target)
    plutarch.url = "github:Plutonomicon/plutarch-plutus?ref=e9e9df286768440733890b1260ad569a2f882890";

    # Typescript runtimes
    prelude-typescript.follows = "plutus-ledger-api-typescript/prelude-typescript";
    plutus-ledger-api-typescript = {
      url = "github:mlabs-haskell/plutus-ledger-api-typescript/v1.2.1";
      inputs.flake-lang.follows = "flake-lang";
    };
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./settings.nix
        ./pre-commit.nix
        ./hercules-ci.nix
        ./docs/build.nix
        ./docs/plutarch/build.nix
        ./docs/plutustx/build.nix
        ./extras/build.nix
        ./extras/lbf-nix/build.nix
        ./extras/dev-shells/build.nix
        ./extras/lambda-buffers-utils/build.nix
        ./libs/build.nix
        ./api/build.nix
        ./lambda-buffers-compiler/build.nix
        ./lambda-buffers-codegen/build.nix
        ./lambda-buffers-frontend/build.nix
        ./runtimes/haskell/lbr-prelude/build.nix
        ./runtimes/haskell/lbr-plutus/build.nix
        ./runtimes/haskell/lbr-plutustx/build.nix
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
        ./testsuites/lbt-plutus/lbt-plutus-plutustx/build.nix
        ./testsuites/lbt-plutus/lbt-plutus-rust/build.nix
        ./experimental/build.nix
        ./docs/typescript-prelude/build.nix
        ./docs/typescript-plutus/build.nix
      ];
      debug = true;
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
        "aarch64-linux"
      ];
    };
}
