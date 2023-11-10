_:
{
  perSystem = { config, pkgs, ... }:
    let
      hsFlake = config.lbf-nix.haskellFlake {
        src = ./.;

        name = "lambda-buffers-codegen";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          "${config.packages.lambda-buffers-lang-hs-pb}"
          "${config.packages.lambda-buffers-compiler-hs-pb}"
          "${config.packages.lambda-buffers-codegen-hs-pb}"
          "${config.packages.lambda-buffers-compiler-src}"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };
    in

    {
      devShells.dev-codegen = hsFlake.devShell;

      packages = {

        lambda-buffers-codegen-src = pkgs.stdenv.mkDerivation {
          name = "lambda-buffers-codegen-src";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };
        lambda-buffers-codegen-lib = hsFlake.packages."lambda-buffers-codegen:lib:lambda-buffers-codegen";
        lambda-buffers-codegen-tests = hsFlake.packages."lambda-buffers-codegen:test:tests";
        lambda-buffers-codegen-cli = hsFlake.packages."lambda-buffers-codegen:exe:lbg";
        lbg = config.packages.lambda-buffers-codegen-cli;
        lbg-haskell = pkgs.writeShellScriptBin "lbg-haskell" ''
          ${config.packages.lbg}/bin/lbg gen-haskell $@
        '';
        lbg-purescript = pkgs.writeShellScriptBin "lbg-purescript" ''
          ${config.packages.lbg}/bin/lbg gen-purescript $@
        '';
        lbg-plutarch = pkgs.writeShellScriptBin "lbg-plutarch" ''
          ${config.packages.lbg}/bin/lbg gen-plutarch $@
        '';

        codegen-configs = pkgs.stdenv.mkDerivation {
          name = "codegen-configs";
          src = ./data;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

      };

      inherit (hsFlake) checks;

    };
}
