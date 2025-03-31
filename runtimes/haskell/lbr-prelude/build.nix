{ inputs, ... }:
{
  perSystem =
    {
      config,
      lib,
      pkgs,
      system,
      ...
    }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellFlake {
        src = ./.;

        name = "lbr-prelude";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in

    {
      devShells.dev-lbr-prelude-haskell = hsFlake.devShell;

      packages = {

        lbr-prelude-haskell-src = pkgs.stdenv.mkDerivation {
          name = "lbr-prelude-haskell-src";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

      } // hsFlake.packages;

      inherit (hsFlake) checks;

    };
}
