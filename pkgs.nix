# Repo-wide Nixpkgs with different overlays
{ inputs, ... }:
{
  perSystem = { pkgs, system, ... }: {

    _module.args = {
      pkgs = import inputs.nixpkgs {
        inherit system;
      };

      # TODO(bladyjoker): If we use recent nixpkgs we get: `error: nodejs_14 has been removed as it is EOL`. That's why we use CTL's old nixpkgs.
      pkgsForCtl = import inputs.ctl.inputs.nixpkgs {
        inherit system;
        inherit (inputs.haskell-nix) config;
        overlays = [
          inputs.haskell-nix.overlay
          inputs.iohk-nix.overlays.crypto
          inputs.iohk-nix.overlays.haskell-nix-crypto
          inputs.ctl.overlays.purescript
          inputs.ctl.overlays.spago
        ];
      };

      # TODO(bladyjoker): haskell.nix is brittle on its nixpkgs, and tends to break stuff for us, so we instantiate haskell.nix nixpkgs specifically. For example https://github.com/Plutonomicon/plutarch-plutus/pull/624
      pkgsForHaskellNix = import inputs.haskell-nix.inputs.nixpkgs {
        inherit system;
        inherit (inputs.haskell-nix) config;
        overlays = [
          inputs.haskell-nix.overlay
          inputs.iohk-nix.overlays.crypto
          inputs.iohk-nix.overlays.haskell-nix-crypto
        ];
      };

      pkgsForRust = import inputs.nixpkgs {
        inherit system;
        overlays = [
          (import inputs.rust-overlay)
        ];
      };
    };
  };
}
