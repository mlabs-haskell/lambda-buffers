_:
{
  perSystem = { config, pkgs, ... }:
    let
      hsFlake = config.lbf-nix.haskellPlutusFlake {
        src = ./.;

        name = "lbr-plutus";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [ "${config.packages.lbr-prelude-haskell-src}" ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in
    {
      devShells.dev-lbr-plutus-haskell = hsFlake.devShell;

      packages = {

        lbr-plutus-haskell-src = pkgs.stdenv.mkDerivation {
          name = "lbr-plutus-haskell-src";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

        lbr-plutus-haskell-lib = hsFlake.packages."lbr-plutus:lib:lbr-plutus";
        lbr-plutus-haskell-tests = hsFlake.packages."lbr-plutus:test:tests";
      };

      inherit (hsFlake) checks;

    };
}
