# Build .lbf schemas that use LB Prelude package and targets Haskell's Base (and friends) library.
pkgs: lbf: lbg-haskell: lbfHaskellOpts:
let
  utils = import ./utils.nix pkgs;

  lbfHs = import ./lbf-haskell.nix pkgs lbf lbg-haskell;
  lbfHaskellOptsForPrelude = utils.overrideAttrs
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
        override = cfgs: cfgs ++ [ ../../lambda-buffers-codegen/data/haskell-prelude-base.json ];
      };
    }
    lbfHaskellOpts;

in
lbfHs lbfHaskellOptsForPrelude
