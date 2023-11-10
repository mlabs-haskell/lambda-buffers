_:
{
  perSystem = { pkgs, config, ... }:
    {
      packages = {
        lbt-plutus-golden-haskell = config.lbf-nix.haskellData {
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
