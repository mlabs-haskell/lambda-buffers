# Build .lbf schemas that use LB Prelude package and targets TypeScript Prelude
# (and friends) library.
# Warning: Essentially duplicated code from  `./lbf-prelude-haskell.nix`
# TODO(jaredponn): instead of passing everything in, how about we just "go all
# the way" using flake-parts and make all these scripts their own flake-parts
# module?
{ pkgs
, lbf
, lbg-typescript
, config
, typescriptFlake
}: lbfTypeScriptOpts:
let
  utils = import ./utils.nix pkgs;

  lbfTs = import ./lbf-typescript.nix { inherit pkgs lbf lbg-typescript typescriptFlake; };
  lbfTypeScriptOptsForPrelude = utils.overrideAttrs
    {
      imports = {
        default = { };
        override = libs: libs // { lbf-prelude = ../../libs/lbf-prelude; };
      };
      npmExtraDependencies = {
        default = [ ];
        override = deps: deps ++
          [
            config.packages.lbf-prelude-typescript
          ];
      };
      classes = {
        default = [ ];
        override = cls: cls ++ [ "Prelude.Eq" "Prelude.Json" ];
      };
      configs = {
        default = [ ];
        override = cfgs: cfgs ++ [ ../../lambda-buffers-codegen/data/typescript-prelude-base.json ];
      };
    }
    lbfTypeScriptOpts;

in
lbfTs lbfTypeScriptOptsForPrelude
