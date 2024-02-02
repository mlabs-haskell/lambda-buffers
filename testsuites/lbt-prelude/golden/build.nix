{ inputs, ... }:
{
  perSystem = { pkgs, config, system, ... }:
    {
      devShells.dev-lbt-prelude-golden = config.devShells.default;

      packages = {
        lbt-prelude-golden-haskell = inputs.flake-lang.lib.${system}.haskellData {
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

        lbt-prelude-golden-rust = pkgs.stdenv.mkDerivation {
          name = "lbt-prelude-golden-data";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };
      };

    };
}
