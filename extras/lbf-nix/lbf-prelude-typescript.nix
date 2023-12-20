# Build .lbf schemas that use LB Prelude package and targets TypeScript Prelude
# (and friends) library.
# Warning: Essentially duplicated code from  `./lbf-prelude-haskell.nix`
# TODO(jaredponn): instead of passing everything in, how about we just "go all
# the way" using flake-parts and make all these scripts their own flake-parts
# module?
pkgs: lbf: lbg-typescript: config: inputs': lbfTypeScriptOpts:
let
  utils = import ./utils.nix pkgs;

  lbfTs = import ./lbf-typescript.nix pkgs lbf lbg-typescript;
  lbfTypeScriptOptsForPrelude = utils.overrideAttrs
    {
      imports = {
        default = [ ];
        override = libs: libs ++ [ ../../libs/lbf-prelude ];
      };
      npmDependencies = {
        default = [ ];
        override = deps: deps ++
          [
            inputs'.prelude-typescript.packages.tgz
            config.packages.lbr-prelude-typescript-tgz
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
