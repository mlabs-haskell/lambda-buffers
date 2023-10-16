self@{ inputs, ... }:
{
  perSystem = { pkgs, system, inputs', config, ... }:
    let
      project = { lib, ... }: {
        src = ./.;

        name = "lambda-buffers-codegen";

        inherit (self.config.settings.haskell) index-state compiler-nix-name;

        extraHackage = [
          "${config.packages.lambda-buffers-lang-hs-pb}"
          "${config.packages.lambda-buffers-compiler-hs-pb}"
          "${config.packages.lambda-buffers-codegen-hs-pb}"
          "${config.packages.lambda-buffers-compiler-src}"
        ];

        modules = [
          (_: {
            packages = {
              allComponent.doHoogle = true;
              allComponent.doHaddock = true;

              # Enable strict compilation
              lambda-buffers-codegen.configureFlags = [ "-f-dev" ];
            };
          })
        ];

        shell = {

          withHoogle = true;

          exactDeps = true;

          #nativeBuildInputs = builtins.attrValues commonTools;

          tools = {
            cabal = { };
            haskell-language-server = { };
          };

          shellHook = lib.mkForce ''
            export LC_CTYPE=C.UTF-8;
            export LC_ALL=C.UTF-8;
            export LANG=C.UTF-8;
            ${config.pre-commit.installationScript}
          '';
        };
      };
      hsNixFlake = (pkgs.haskell-nix.cabalProject' [
        inputs.mlabs-tooling.lib.mkHackageMod
        project
      ]).flake { };

    in

    {
      devShells.dev-codegen = hsNixFlake.devShell;

      packages = {

        lambda-buffers-codegen-src = pkgs.stdenv.mkDerivation {
          name = "lambda-buffers-codegen-src";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };
        lambda-buffers-codegen-lib = hsNixFlake.packages."lambda-buffers-codegen:lib:lambda-buffers-codegen";
        lambda-buffers-codegen-tests = hsNixFlake.packages."lambda-buffers-codegen:test:tests";
        lambda-buffers-codegen-cli = hsNixFlake.packages."lambda-buffers-codegen:exe:lbg";
        lbg = config.packages.lambda-buffers-codegen-cli;
        lbg-haskell = pkgs.writeShellScriptBin "lbg-haskell" ''
          ${config.packages.lbg}/bin/lbg gen-haskell $@
        '';
        lbg-purescript = pkgs.writeShellScriptBin "lbg-purescript" ''
          ${config.packages.lbg}/bin/lbg gen-purescript $@
        '';

        codegen-configs = pkgs.stdenv.mkDerivation {
          name = "codegen-configs";
          src = ./data;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

      };

      inherit (hsNixFlake) checks;

    };
}
