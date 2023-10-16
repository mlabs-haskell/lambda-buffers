# Build .lbf schemas that use LB Plutus (and by extension LB Prelude) package and targets Purescript's CTL library.
pkgs: lbf: lbg-purescript: lbfPurescriptOpts:
let
  utils = import ./utils.nix pkgs;

  lbfPurs = import ./lbf-purescript.nix pkgs lbf lbg-purescript;
  lbfPurescriptOptsForPlutus = utils.overrideAttrs
    {
      imports = {
        default = [ ];
        override = libs: libs ++ [ ../../libs/lbf-plutus ../../libs/lbf-prelude ];
      };
      dependencies = {
        default = [ ];
        override = deps: deps ++ [ "lbf-plutus" "lbf-prelude" ];
      };
      classes = {
        default = [ ];
        override = _: [ "Prelude.Eq" "Plutus.V1.PlutusData" ]; # TODO(bladyjoker): When Json instances are implemented for CTL Plutus types, bring back Json.
      };
      configs = {
        default = [ ];
        override = cfgs: cfgs ++ [ ../../lambda-buffers-codegen/data/purescript-prelude-base.json ../../lambda-buffers-codegen/data/purescript-plutus-ctl.json ];
      };
    }
    lbfPurescriptOpts;
in
lbfPurs lbfPurescriptOptsForPlutus
