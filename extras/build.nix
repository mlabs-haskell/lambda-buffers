# TODO(bladyjoker): Using overlayAttrs here as a hack to share functions -.- Do this properly.
{ inputs, ... }: {
  imports = [
    inputs.flake-parts.flakeModules.easyOverlay # Adds perSystem.overlayAttrs
  ];
  perSystem = { pkgs, ... }:
    {

      overlayAttrs = {
        extras = {
          purescriptFlake = import ./flake-purescript.nix pkgs;
          rustFlake = import ./flake-rust.nix pkgs;
          typescriptFlake = import ./flake-typescript.nix pkgs;
          haskellData = import ./haskell-data.nix pkgs;
        };
      };

    };
}
