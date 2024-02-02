{ inputs, ... }:
{
  perSystem = { pkgs, system, ... }:
    {
      packages = {
        lbt-plutus-golden-haskell = inputs.flake-lang.lib.${system}.haskellData {
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

        lbt-plutus-golden-rust = pkgs.stdenv.mkDerivation {
          name = "lbt-plutus-golden-data";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };
      };

    };
}
