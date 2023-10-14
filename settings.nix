# Repo wide settings
{ lib, ... }: {
  options = {

    settings = {

      haskell = {

        index-state = lib.mkOption {
          type = lib.types.str;
          description = "Hackage index state to use when making a haskell.nix build environment";
        };

        compiler-nix-name = lib.mkOption {
          type = lib.types.str;
          description = "GHC Haskell compiler to use when building haskell.nix projects";
        };

      };

    };

  };

  config.settings.haskell.index-state = "2022-12-01T00:00:00Z";
  config.settings.haskell.compiler-nix-name = "ghc925";
}
