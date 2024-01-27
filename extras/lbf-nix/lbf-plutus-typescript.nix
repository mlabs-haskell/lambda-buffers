# Build .lbf schemas that use LB Plutus (and by extension LB Prelude) package and targets Typescript's plutus-ledger-api library.
{ pkgs
, lbf
, lbg-typescript
, config
, typescriptFlake
}:
lbfTypescriptOpts:
let
  utils = import ./utils.nix pkgs;

  lbfTypescript = import ./lbf-typescript.nix { inherit pkgs lbf lbg-typescript typescriptFlake; };
  lbfTypescriptOptsForPlutus = utils.overrideAttrs
    {
      imports = {
        default = { };
        override = libs: libs // {
          lbf-prelude = "${config.packages.lbf-prelude}";
          lbf-plutus = "${config.packages.lbf-plutus}";
        };
      };
      npmExtraDependencies = {
        default = [ ];
        override = deps: deps ++ [
          config.packages.lbf-plutus-typescript
        ];
      };
      classes = {
        default = [ ];
        override = cls: cls ++ [ "Prelude.Eq" "Prelude.Json" "Plutus.V1.PlutusData" ];
      };
      configs = {
        default = [ ];
        override = _: [
          "${config.packages.codegen-configs}/typescript-prelude-base.json"
          "${config.packages.codegen-configs}/typescript-plutus.json"
        ];
      };
    }
    lbfTypescriptOpts;
in
lbfTypescript lbfTypescriptOptsForPlutus
