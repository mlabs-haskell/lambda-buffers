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
      };

    };
}
