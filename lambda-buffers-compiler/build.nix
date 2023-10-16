self@{ inputs, ... }:
{
  perSystem = { pkgs, system, inputs', config, ... }:
    let
      project = { lib, ... }: {
        src = ./.;

        name = "lambda-buffers-compiler";

        inherit (self.config.settings.haskell) index-state compiler-nix-name;

        extraHackage = [
          "${config.packages.lambda-buffers-lang-hs-pb}"
          "${config.packages.lambda-buffers-compiler-hs-pb}"
          "${config.packages.lambda-buffers-codegen-hs-pb}"
        ];

        modules = [
          (_: {
            packages = {
              allComponent.doHoogle = true;
              allComponent.doHaddock = true;

              # Enable strict compilation
              lambda-buffers-compiler.configureFlags = [ "-f-dev" ];
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
      devShells.dev-compiler = hsNixFlake.devShell;

      packages = {

        lambda-buffers-compiler-src = pkgs.stdenv.mkDerivation {
          name = "lambda-buffers-compiler-src";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

        lambda-buffers-compiler-lib = hsNixFlake.packages."lambda-buffers-compiler:lib:lambda-buffers-compiler";
        lambda-buffers-compiler-tests = hsNixFlake.packages."lambda-buffers-compiler:test:tests";
        lambda-buffers-compiler-cli = hsNixFlake.packages."lambda-buffers-compiler:exe:lbc";
        lbc = config.packages.lambda-buffers-compiler-cli;

      };

      inherit (hsNixFlake) checks;

    };
}
