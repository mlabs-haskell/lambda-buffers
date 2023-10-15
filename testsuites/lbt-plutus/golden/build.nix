{ inputs, lib, ... }:
{
  perSystem = { pkgs, system, inputs', config, ... }:
    {
      packages = {
        lbt-plutus-golden-haskell = config.overlayAttrs.extras.haskellData {
          srcs = [ ./. ];
          cabalDataPatterns = [ "**/*.json" ];
          cabalPackageName = "lbt-plutus-golden-data";
        };

        lbt-plutus-golden-purescript = pkgs.stdenv.mkDerivation {
          name = "lbt-plutus-golden-data";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };
      };

    };
}
