# Build .lbf schemas that use LB Plutus (and by extension LB Prelude) package and targets Purescript's CTL library.
pkgs: lbf: lbg-purescript: lbfPurescriptOpts:
let
  utils = import ./utils.nix pkgs;

  lbfPurs = import ./lbf-prelude-purescript.nix pkgs lbf lbg-purescript;
  lbfPurescriptOptsForPlutus = utils.overrideAttrs
    {
      imports = {
        default = [ ];
        override = libs: libs ++ [ ../../libs/lbf-plutus ];
      };
      dependencies = {
        default = [ ];
        override = deps: deps ++ [ "lbf-plutus" ];
      };
      classes = {
        default = [ ];
        override = cls: cls ++ [ "Plutus.V1.PlutusData" ];
      };
      configs = {
        default = [ ];
        override = cfgs: cfgs ++ [ ../../lambda-buffers-codegen/data/purescript-plutus-ctl.json ];
      };
    }
    lbfPurescriptOpts;
in
lbfPurs lbfPurescriptOptsForPlutus
