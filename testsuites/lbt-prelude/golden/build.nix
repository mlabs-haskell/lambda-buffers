_:
{
  perSystem = { pkgs, config, ... }:
    {
      devShells.dev-lbt-prelude-golden = config.devShells.dev-pre-commit;

      packages = {
        lbt-prelude-golden-haskell = config.lbf-nix.haskellData {
          srcs = [ ./. ];
          cabalDataPatterns = [ "**/*.json" ];
          cabalPackageName = "lbt-prelude-golden-data";
        };

        lbt-prelude-golden-purescript = pkgs.stdenv.mkDerivation {
          name = "lbt-prelude-golden-data";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };
      };

    };
}
