# Build .lbf schemas that use LB Plutus (and by extension LB Prelude) package and targets Haskell's PlutusTx language (not library).
pkgs: lbf: lbg-plutustx: lbfPlutusTxOpts:
let
  utils = import ./utils.nix pkgs;

  lbfPlutusTx = import ./lbf-plutustx-base.nix pkgs lbf lbg-plutustx;
  lbfPlutusTxOptsForPlutus = utils.overrideAttrs
    {
      imports = {
        default = [ ];
        override = libs: libs ++ [ ../../libs/lbf-prelude ../../libs/lbf-plutus ];
      };
      dependencies = {
        default = [ ];
        override = deps: deps ++ [ "lbf-prelude-plutustx" "lbf-plutus-plutustx" ];
      };
      classes = {
        default = [ ];
        override = cls: cls ++ [ "Prelude.Eq" "Plutus.V1.PlutusData" ];
      };
      configs = {
        default = [ ];
        override = _: [ ../../lambda-buffers-codegen/data/plutustx-prelude.json ../../lambda-buffers-codegen/data/plutustx-plutus.json ];
      };
    }
    lbfPlutusTxOpts;
in
lbfPlutusTx lbfPlutusTxOptsForPlutus
