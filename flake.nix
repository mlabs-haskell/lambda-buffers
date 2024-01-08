{
  description = "Lambda Buffers";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    # Haskell

    ## Using haskell.nix to build Haskell projects
    haskell-nix.url = "github:input-output-hk/haskell.nix";

    # Nix

    ## Flakes as modules, using this extensively to organize the repo into modules (build.nix files)
    flake-parts.url = "github:hercules-ci/flake-parts";

    ## Code quality automation
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    ## Hercules CI effects
    hci-effects.url = "github:hercules-ci/hercules-ci-effects";

    ## Nix library for Google Protobufs
    proto-nix.url = "github:mlabs-haskell/proto.nix";

    # Purescript

    ## Cardano transaction library (leveraging CTL's Purescript Nix machinery)
    ctl.url = "github:plutonomicon/cardano-transaction-lib?ref=develop";

    # Rust

    crane.url = "github:ipetkov/crane";
    rust-overlay.url = "github:oxalica/rust-overlay";

    # Plutus

    ## CHaP is a custom hackage for Plutus development
    cardano-haskell-packages.url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
    cardano-haskell-packages.flake = false;

    ## Some crypto overlays necessary for Plutus
    iohk-nix.url = "github:input-output-hk/iohk-nix";

    ## Foundational Plutus library
    plutus.url = "github:input-output-hk/plutus";

    ## Plutarch eDSL that LB generates to
    plutarch = {
      url = "github:plutonomicon/plutarch-plutus";
      flake = false;
    };

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
        ./runtimes/rust/lbr-prelude-derive/build.nix
        ./runtimes/typescript/lbr-prelude/build.nix
        ./runtimes/typescript/lbr-plutus/build.nix
        ./testsuites/lbt-prelude/api/build.nix
        ./testsuites/lbt-prelude/golden/build.nix
        ./testsuites/lbt-prelude/lbt-prelude-haskell/build.nix
        ./testsuites/lbt-prelude/lbt-prelude-purescript/build.nix
        ./testsuites/lbt-prelude/lbt-prelude-rust/build.nix
        ./testsuites/lbt-plutus/api/build.nix
        ./testsuites/lbt-plutus/golden/build.nix
        ./testsuites/lbt-plutus/lbt-plutus-haskell/build.nix
        ./testsuites/lbt-plutus/lbt-plutus-purescript/build.nix
        ./testsuites/lbt-plutus/lbt-plutus-plutarch/build.nix
        ./testsuites/lbt-plutus/lbt-plutus-rust/build.nix
        ./experimental/build.nix
      ];
      debug = true;
      systems = [ "x86_64-linux" "x86_64-darwin" ];
    };
}
