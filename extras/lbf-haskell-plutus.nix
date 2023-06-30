let
  utils = import ./utils.nix;
in
lbf: lbg-haskell: lbfHaskellOpts: (import ./lbf-haskell.nix) lbf lbg-haskell (utils.overrideAttrs
{
  "imports" = {
    default = [ ];
    override = imps: imps ++ [ ../libs/lbf-plutus ../libs/lbf-prelude ];
  };
  "dependencies" = {
    default = [ ];
    override = deps: deps ++ [ "lbf-prelude" "lbr-prelude" "lbf-plutus" "lbr-plutus" ];
  };
  "classes" = {
    default = [ ];
    override = classes: classes ++ [ "Plutus.V1.PlutusData" "Prelude.Eq" "Prelude.Json" ];
  };
}
  lbfHaskellOpts
)

  
