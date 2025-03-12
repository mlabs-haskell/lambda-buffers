# Build .lbf schemas that use LB Plutus (and by extension LB Prelude) package and targets Rust's plutus-ledger-api library.
pkgs: lbf: lbg-rust: lbfRustOpts:
let
  utils = import ./utils.nix pkgs;

  lbfRust = import ./lbf-prelude-rust.nix pkgs lbf lbg-rust;
  lbfRustOptsForPlutus = utils.overrideAttrs {
    imports = {
      default = { };
      override =
        libs:
        libs
        // {
          lbf-plutus = ../../libs/lbf-plutus;
        };
    };
    classes = {
      default = [ ];
      override = cls: cls ++ [ "Plutus.V1.PlutusData" ];
    };
    configs = {
      default = [ ];
      override = cfgs: cfgs ++ [ ../../lambda-buffers-codegen/data/rust-plutus-pla.json ];
    };
  } lbfRustOpts;
in
lbfRust lbfRustOptsForPlutus
