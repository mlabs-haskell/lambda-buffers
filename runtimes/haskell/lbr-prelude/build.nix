self@{ inputs, ... }:
{
  perSystem = { pkgs, config, ... }:
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

          nativeBuildInputs = config.settings.shell.tools;

          tools = {
            cabal = { };
            haskell-language-server = { };
          };

          shellHook = lib.mkForce config.settings.shell.hook;
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
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

      } // hsNixFlake.packages;

      inherit (hsNixFlake) checks;

    };
}
