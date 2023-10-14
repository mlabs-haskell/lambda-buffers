{ inputs, lib, ... }:
{
  perSystem = { pkgs, system, inputs', config, ... }:
    {
      packages = {
        lbt-plutus-golden-haskell = import ../../../extras/haskell-data.nix {
          inherit pkgs;
          srcs = [ ./. ];
          cabalDataPatterns = [ "**/*.json" ];
          cabalPackageName = "lbt-plutus-golden-data";
        };
      };

    };
}
