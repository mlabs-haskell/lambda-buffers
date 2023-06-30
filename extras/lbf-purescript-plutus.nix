let
  utils = import ./utils.nix;
in
lbf: lbg-purescript: lbfPurescriptOpts: (import ./lbf-purescript.nix) lbf lbg-purescript (utils.overrideAttrs
{
  "imports" = {
    default = [ ];
    override = imps: imps ++ [ ../libs/lbf-plutus ../libs/lbf-prelude ];
  };
  "dependencies" = {
    default = [ ];
    override = deps: deps ++ [ "lbf-prelude" "lbr-prelude" "lbf-plutus" "lbr-plutus" ];
  };
  # TODO(https://github.com/mlabs-haskell/lambda-buffers/issues/98): Add Prelude.Json once the issue is resolved
  "classes" = {
    default = [ ];
    override = classes: classes ++ [ "Plutus.V1.PlutusData" "Prelude.Eq" ];
  };
}
  lbfPurescriptOpts
)
