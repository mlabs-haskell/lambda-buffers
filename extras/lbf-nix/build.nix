# TODO(bladyjoker): Using overlayAttrs here as a hack to share functions -.- Do this properly.
{ inputs, ... }: {
  imports = [
    inputs.flake-parts.flakeModules.easyOverlay # Adds perSystem.overlayAttrs
  ];
  perSystem = { pkgs, config, ... }:
    {

      overlayAttrs = {
        lbf-nix = {
          lbfBuild = import ./lbf-build.nix pkgs config.packages.lbf;
          lbfHaskell = import ./lbf-haskell.nix pkgs config.packages.lbf config.packages.lbg-haskell;
          lbfPreludeHaskell = import ./lbf-prelude-hs.nix pkgs config.packages.lbf config.packages.lbg-haskell;
          lbfPlutusHaskell = import ./lbf-plutus-hs-plutustx.nix pkgs config.packages.lbf config.packages.lbg-haskell;
          lbfPlutarch' = import ./lbf-plutarch.nix pkgs config.packages.lbf config.packages.lbg-plutarch;
          lbfPlutarch = import ./lbf-plutus-plutarch.nix pkgs config.packages.lbf config.packages.lbg-plutarch;
          lbfPurescript = import ./lbf-purescript.nix pkgs config.packages.lbf config.packages.lbg-purescript;
          lbfPreludePurescript = import ./lbf-prelude-purescript.nix pkgs config.packages.lbf config.packages.lbg-purescript;
          lbfPlutusPurescript = import ./lbf-plutus-purescript.nix pkgs config.packages.lbf config.packages.lbg-purescript;
        };
      };

    };
}
