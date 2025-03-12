# Build .lbf schemas that use LB Plutus (and by extension LB Prelude) package and targets Haskell's Plutarch library.
pkgs: lbf: lbg-plutarch: lbfPlutarchOpts:
let
  utils = import ./utils.nix pkgs;

  lbfPlutarch = import ./lbf-plutarch-base.nix pkgs lbf lbg-plutarch;
  lbfPlutarchOptsForPlutus = utils.overrideAttrs {
    imports = {
      default = [ ];
      override =
        libs:
        libs
        ++ [
          ../../libs/lbf-prelude
          ../../libs/lbf-plutus
        ];
    };
    dependencies = {
      default = [ ];
      override =
        deps:
        deps
        ++ [
          "lbf-prelude-plutarch"
          "lbf-plutus-plutarch"
        ];
    };
    classes = {
      default = [ ];
      override =
        cls:
        cls
        ++ [
          "Prelude.Eq"
          "Plutus.V1.PlutusData"
        ];
    };
    configs = {
      default = [ ];
      override = _: [
        ../../lambda-buffers-codegen/data/plutarch-prelude.json
        ../../lambda-buffers-codegen/data/plutarch-plutus.json
      ];
    };
  } lbfPlutarchOpts;
in
lbfPlutarch lbfPlutarchOptsForPlutus
