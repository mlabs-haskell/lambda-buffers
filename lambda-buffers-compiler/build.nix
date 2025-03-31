{ inputs, ... }:
{
  perSystem =
    {
      config,
      pkgs,
      system,
      ...
    }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellFlake {
        src = ./.;

        name = "lambda-buffers-compiler";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          "${config.packages.lambda-buffers-lang-hs-pb}"
          "${config.packages.lambda-buffers-compiler-hs-pb}"
          "${config.packages.lambda-buffers-codegen-hs-pb}"
          "${config.packages.lambda-buffers-utils-src}"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };
    in

    {
      devShells.dev-lambda-buffers-compiler = hsFlake.devShell;

      packages = {

        lambda-buffers-compiler-src = pkgs.stdenv.mkDerivation {
          name = "lambda-buffers-compiler-src";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

        lambda-buffers-compiler-lib =
          hsFlake.packages."lambda-buffers-compiler:lib:lambda-buffers-compiler";
        lambda-buffers-compiler-tests = hsFlake.packages."lambda-buffers-compiler:test:tests";
        lambda-buffers-compiler-cli = hsFlake.packages."lambda-buffers-compiler:exe:lbc";
        lbc = config.packages.lambda-buffers-compiler-cli;

      };

      inherit (hsFlake) checks;

    };
}
