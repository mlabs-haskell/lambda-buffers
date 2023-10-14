# Foundational .lbf packages
# TODO(bladyjoker): Make packages that actually try and compile.
{ inputs, lib, ... }:
{
  perSystem = { pkgs, system, inputs', config, ... }: {

    packages = {
      lbf-prelude-haskell = config.overlayAttrs.lbf-nix.lbfHaskell {
        name = "lbf-prelude";
        src = ./lbf-prelude;
        files = [ "Prelude.lbf" ];
        classes = [ "Prelude.Eq" "Prelude.Json" ];
        configs = [ ../lambda-buffers-codegen/data/haskell-prelude-base.json ];
      };

      lbf-plutus-haskell = config.overlayAttrs.lbf-nix.lbfHaskell {
        name = "lbf-plutus";
        src = ./lbf-plutus;
        imports = [ ./lbf-prelude ];
        files = [ "Plutus/V1.lbf" "Plutus/V2.lbf" ];
        classes = [ "Prelude.Eq" "Prelude.Json" "Plutus.V1.PlutusData" ];
        dependencies = [ "lbf-prelude" ];
        configs = [ ../lambda-buffers-codegen/data/haskell-prelude-base.json ../lambda-buffers-codegen/data/haskell-plutus-plutustx.json ];
      };

    };

  };
}

