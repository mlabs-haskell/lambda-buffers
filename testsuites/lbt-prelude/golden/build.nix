_:
{
  perSystem = { pkgs, config, ... }:

    let
      goldenData = pkgs.stdenv.mkDerivation {
        name = "lbt-prelude-golden-data";
        src = ./.;
        # Disable the Fixup phase since it needs to (potentially) write to the
        # files in `./.` which are readonly in the nix store
        dontFixup = true;
        installPhase = ''ln -s "$src" "$out"'';
      };
    in
    {
      devShells.dev-lbt-prelude-golden = config.devShells.default;

      packages = {
        lbt-prelude-golden-haskell = config.lbf-nix.haskellData {
          srcs = [ ./. ];
          cabalDataPatterns = [ "**/*.json" ];
          cabalPackageName = "lbt-prelude-golden-data";
        };

        lbt-prelude-golden-purescript = goldenData;

        lbt-prelude-golden-rust = goldenData;

        lbt-prelude-golden-typescript = goldenData;

      };
    };
}
