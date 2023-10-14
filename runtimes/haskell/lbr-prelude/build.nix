self@{ inputs, ... }:
{
  perSystem = { pkgs, system, inputs', config, ... }:
    let
      project = { lib, ... }: {
        src = ./.;

        name = "lbr-prelude";

        inherit (self.config.settings.haskell) index-state compiler-nix-name;

        extraHackage = [ ];

        modules = [
          (_: {
            packages = {
              allComponent.doHoogle = true;
              allComponent.doHaddock = true;

              # Enable strict compilation
              lbr-prelude.configureFlags = [ "-f-dev" ];
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
            export LC_CTYPE=C.UTF-8
            export LC_ALL=C.UTF-8
            export LANG=C.UTF-8
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
      devShells.dev-lbr-prelude-haskell = hsNixFlake.devShell;

      packages = {

        lbr-prelude-haskell-src = pkgs.stdenv.mkDerivation {
          name = "lbr-prelude-haskell-src";
          src = ./.;
          installPhase = ''mkdir $out; cp -r $src/* $out;'';
        };

      } // hsNixFlake.packages;

      inherit (hsNixFlake) checks;

    };
}
