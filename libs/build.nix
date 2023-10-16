# Foundational .lbf packages
# TODO(bladyjoker): Make packages that actually try and compile.
_:
{
  perSystem = { pkgs, config, ... }: {

    packages = {
      lbf-prelude = pkgs.stdenv.mkDerivation {
        name = "lbf-prelude";
        src = ./lbf-prelude;
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

      lbf-prelude-haskell = config.overlayAttrs.lbf-nix.lbfHaskell {
        name = "lbf-prelude";
        src = ./lbf-prelude;
        files = [ "Prelude.lbf" ];
        classes = [ "Prelude.Eq" "Prelude.Json" ];
        configs = [ ../lambda-buffers-codegen/data/haskell-prelude-base.json ];
      };

      lbf-prelude-purescript = config.overlayAttrs.lbf-nix.lbfPurescript {
        name = "lbf-prelude";
        src = ./lbf-prelude;
        files = [ "Prelude.lbf" ];
        classes = [ "Prelude.Eq" "Prelude.Json" ];
        configs = [ ../lambda-buffers-codegen/data/purescript-prelude-base.json ];
      };

      lbf-plutus = pkgs.stdenv.mkDerivation {
        name = "lbf-plutus";
        src = ./lbf-plutus;
        phases = "installPhase";
        installPhase = "ln -s $src $out";
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

      lbf-plutus-purescript = config.overlayAttrs.lbf-nix.lbfPurescript {
        name = "lbf-plutus";
        src = ./lbf-plutus;
        imports = [ ./lbf-prelude ];
        files = [ "Plutus/V1.lbf" "Plutus/V2.lbf" ];
        classes = [ "Prelude.Eq" "Prelude.Json" "Plutus.V1.PlutusData" ];
        dependencies = [ "lbf-prelude" ];
        configs = [ ../lambda-buffers-codegen/data/purescript-prelude-base.json ../lambda-buffers-codegen/data/purescript-plutus-ctl.json ];
      };

    };

  };
}

