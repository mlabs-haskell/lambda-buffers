# Build .lbf schemas that use LB Prelude package and targets Purescript's prelude (and friends) library.
pkgs: lbf: lbg-purescript: lbfPurescriptOpts:
let
  utils = import ./utils.nix pkgs;

  lbfPurs = import ./lbf-purescript.nix pkgs lbf lbg-purescript;
  lbfPurescriptOptsForPrelude = utils.overrideAttrs
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
        override = cfgs: cfgs ++ [ ../../lambda-buffers-codegen/data/purescript-prelude-base.json ];
      };
    }
    lbfPurescriptOpts;

in
lbfPurs lbfPurescriptOptsForPrelude
