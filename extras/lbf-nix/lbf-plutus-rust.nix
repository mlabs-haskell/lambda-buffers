# Build .lbf schemas that use LB Plutus (and by extension LB Prelude) package and targets Rust's plutus-ledger-api library.
pkgs: lbf: lbg-rust: lbfRustOpts:
let
  utils = import ./utils.nix pkgs;

  lbfRust = import ./lbf-rust.nix pkgs lbf lbg-rust;
  lbfRustOptsForPlutus = utils.overrideAttrs
    {
      imports = {
        default = { };
        override = libs: libs // {
          lbf-prelude = ../../libs/lbf-prelude;
          lbf-plutus = ../../libs/lbf-plutus;
        };
      };
      classes = {
        default = [ ];
        override = cls: cls ++ [ "Prelude.Eq" "Plutus.V1.PlutusData" ];
      };
      configs = {
        default = [ ];
        override = _: [ ../../lambda-buffers-codegen/data/rust-prelude-base.json ../../lambda-buffers-codegen/data/rust-plutus-pla.json ];
      };
    }
    lbfRustOpts;
in
lbfRust lbfRustOptsForPlutus
