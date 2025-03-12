# Build .lbf schemas that use LB Plutus (and by extension LB Prelude) package and targets Haskell's PlutusTx library.
pkgs: lbf: lbg-haskell: lbfHaskellOpts:
let
  utils = import ./utils.nix pkgs;

  lbfHs = import ./lbf-prelude-hs.nix pkgs lbf lbg-haskell;
  lbfHaskellOptsForPlutus = utils.overrideAttrs {
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
      override = cfgs: cfgs ++ [ ../../lambda-buffers-codegen/data/haskell-plutus-plutustx.json ];
    };
  } lbfHaskellOpts;
in
lbfHs lbfHaskellOptsForPlutus
