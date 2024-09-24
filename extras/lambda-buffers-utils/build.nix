{ inputs, ... }:
{
  perSystem = { config, pkgs, system, ... }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellFlake {
        src = ./.;

        name = "lambda-buffers-utils";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };
    in

    {
      devShells.dev-utils = hsFlake.devShell;

      packages = {

        lambda-buffers-utils-src = pkgs.stdenv.mkDerivation {
          name = "lambda-buffers-utils-src";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

        lambda-buffers-utils-lib = hsFlake.packages."lambda-buffers-utils:lib:lambda-buffers-utils";
        lambda-buffers-utils-tests = hsFlake.packages."lambda-buffers-utils:test:tests";
      };

      inherit (hsFlake) checks;

    };
}
