{ inputs, ... }:
{
  perSystem = { config, pkgs, system, ... }:
    let
      # TODO: plutusTxFlake
      hsFlake = inputs.flake-lang.lib.${system}.haskellPlutusFlake {
        src = ./.;

        name = "lbr-plutustx";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in
    {
      devShells.dev-lbr-plutustx = hsFlake.devShell;

      packages = {

        lbr-plutustx-src = pkgs.stdenv.mkDerivation {
          name = "lbr-plutustx-src";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

        lbr-plutustx-lib = hsFlake.packages."lbr-plutustx:lib:lbr-plutustx";
      };

      inherit (hsFlake) checks;

    };
}
