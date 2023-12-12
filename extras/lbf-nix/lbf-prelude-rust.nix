# Build .lbf schemas that use LB Prelude package and targets Rust's std (and friends) library.
pkgs: lbf: lbg-rust: lbfRustOpts:
let
  utils = import ./utils.nix pkgs;

  lbfRs = import ./lbf-rust.nix pkgs lbf lbg-rust;
  lbfRustOptsForPrelude = utils.overrideAttrs
    {
      imports = {
        default = [ ];
        override = libs: libs ++ [ ../../libs/lbf-prelude ];
      };
      dependencies = {
        default = [ ];
        override = deps: deps ++ [ "lbf-prelude" ];
      };
      classes = {
        default = [ ];
        override = cls: cls ++ [ "Prelude.Eq" "Prelude.Json" ];
      };
      configs = {
        default = [ ];
        override = cfgs: cfgs ++ [ ../../lambda-buffers-codegen/data/rust-prelude-base.json ];
      };
    }
    lbfRustOpts;

in
lbfRs lbfRustOptsForPrelude
