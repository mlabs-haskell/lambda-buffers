{ inputs, ... }:
{
  perSystem =
    { pkgs, system, ... }:
    let
      goldenData = pkgs.stdenv.mkDerivation {
        name = "lbt-plutus-golden-data";
        src = ./.;
        # Disable the Fixup phase since it needs to (potentially) write to the
        # files in `./.` which are readonly in the nix store
        dontFixup = true;
        installPhase = ''ln -s "$src" "$out"'';
      };

    in
    {
      packages = {
        lbt-plutus-golden-haskell = inputs.flake-lang.lib.${system}.haskellData {
          srcs = [ ./. ];
          cabalDataPatterns = [ "**/*.json" ];
          cabalPackageName = "lbt-plutus-golden-data";
        };

        lbt-plutus-golden-purescript = goldenData;

        lbt-plutus-golden-rust = goldenData;

        lbt-plutus-golden-typescript = goldenData;
      };

    };
}
