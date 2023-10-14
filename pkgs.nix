# Repo-wide Nixpkgs with a ton of overlays
{ inputs, lib, ... }:
{
  perSystem = { pkgs, system, inputs', config, ... }: {

    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      inherit (inputs.haskell-nix) config;
      overlays = [
        inputs.haskell-nix.overlay
        inputs.iohk-nix.overlays.crypto
        inputs.ctl.overlays.purescript
        inputs.ctl.overlays.spago
      ];
    };

  };
}
