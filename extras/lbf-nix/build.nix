# TODO(bladyjoker): Using overlayAttrs here as a hack to share functions -.- Do this properly.
{ inputs, ... }: {
  imports = [
    inputs.flake-parts.flakeModules.easyOverlay # Adds perSystem.overlayAttrs
  ];
  perSystem = { pkgs, config, ... }:
    let
      lbg-haskell = "${config.packages.lbg-haskell}/bin/lbg-haskell";
      lbg-plutarch = "${config.packages.lbg-plutarch}/bin/lbg-plutarch";
      lbg-purescript = "${config.packages.lbg-purescript}/bin/lbg-purescript";

    in
    {

      overlayAttrs = {
        lbf-nix = {
          lbfBuild = import ./lbf-build.nix pkgs config.packages.lbf;
          lbfHaskell = import ./lbf-haskell.nix pkgs config.packages.lbf lbg-haskell;
          lbfPreludeHaskell = import ./lbf-prelude-hs.nix pkgs config.packages.lbf lbg-haskell;
          lbfPlutusHaskell = import ./lbf-plutus-hs-plutustx.nix pkgs config.packages.lbf lbg-haskell;
          lbfPlutarch' = import ./lbf-plutarch.nix pkgs config.packages.lbf lbg-plutarch;
          lbfPlutarch = import ./lbf-plutus-plutarch.nix pkgs config.packages.lbf lbg-plutarch;
          lbfPurescript = import ./lbf-purescript.nix pkgs config.packages.lbf lbg-purescript;
          lbfPreludePurescript = import ./lbf-prelude-purescript.nix pkgs config.packages.lbf lbg-purescript;
          lbfPlutusPurescript = import ./lbf-plutus-purescript.nix pkgs config.packages.lbf lbg-purescript;
        };
      };

    };
}
