# Build .lbf schemas that use LB Plutus (and by extension LB Prelude) package and targets Typescript's plutus-ledger-api library.
{ pkgs
, lbf
, lbg-typescript
, config
, typescriptFlake
, lbf-list-modules-typescript
}:
lbfTypescriptOpts:
let
  utils = import ./utils.nix pkgs;

  lbfTypescript = import ./lbf-prelude-typescript.nix { inherit pkgs lbf lbg-typescript config lbf-list-modules-typescript typescriptFlake; };
  lbfTypescriptOptsForPlutus = utils.overrideAttrs
    {
      imports = {
        default = { };
        override = libs: libs // {
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
        override = cls: cls ++ [ "Plutus.V1.PlutusData" ];
      };
      configs = {
        default = [ ];
        override = cfgs: cfgs ++ [ "${config.packages.codegen-configs}/typescript-plutus.json" ];
      };
    }
    lbfTypescriptOpts;
in
lbfTypescript lbfTypescriptOptsForPlutus
