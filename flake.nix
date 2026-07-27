{
  description = "Lambda Buffers";
  inputs = {
    # flake-lang.nix used for monorepo setups
    flake-lang = {
      # Pinned to an immutable rev (not the mutable default branch): Hercules CI
      # re-fetches mutable inputs to their latest during its pure-mode lock update,
      # which no longer matches this repo's lock and forces a re-lock of flake-lang's
      # overridden subtree — that then fails on the (necessarily overridden) hackage
      # input. Pinning flake-lang makes CI use exactly the rev the lock was built
      # against, so no re-lock happens.
      url = "github:mlabs-haskell/flake-lang.nix/f8d33b23dd57cd04560afbdaac601be8c77bae7f";
      # Overridden so haskell.nix can resolve plutus 1.65.0.0 (van Rossem/PV11):
      # flake-lang's own lock pins CHaP and a hackage.nix that predate the 2026-07
      # index-state GHC 9.12 needs (e.g. proto-lens 0.7.1.7). haskell.nix is held at
      # a rev whose GHC 9.12.1 is cache-built (newer haskell.nix pulls a gcc-15
      # nixpkgs that can't build GHC 9.12.1), so its bundled hackage is bumped here.
      inputs = {
        cardano-haskell-packages.url = "github:IntersectMBO/cardano-haskell-packages/f77658bfbf42886478e7a34a1522949cdfc639a3";
        haskell-nix = {
          url = "github:input-output-hk/haskell.nix/7ceff53efc1f6006f68fbfbb496af8720a598152";
          inputs.hackage.url = "github:input-output-hk/hackage.nix/c6c3e35282315c51d8c97c2af3be5cbd4dbc43bc";
        };
      };
    };

    nixpkgs.follows = "flake-lang/nixpkgs";

    # Flakes as modules, using this extensively to organize the repo into modules (build.nix files)
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Code quality automation
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    # Hercules CI effects
    hci-effects.url = "github:hercules-ci/hercules-ci-effects";

    # Nix library for Google Protobufs
    proto-nix = {
      url = "github:mlabs-haskell/proto.nix?ref=szg251/upstream-http2-grpc";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Plutarch eDSL (LB Codegen target)
    plutarch.url = "github:Plutonomicon/plutarch-plutus";

    # Typescript runtimes
    prelude-typescript.follows = "plutus-ledger-api-typescript/prelude-typescript";
    plutus-ledger-api-typescript = {
      url = "github:mlabs-haskell/plutus-ledger-api-typescript/v1.2.2";
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
