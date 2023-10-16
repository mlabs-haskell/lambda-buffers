self@{ inputs, ... }:
{
  perSystem = { pkgs, config, ... }:
    let
      project = { lib, ... }: {
        src = ./.;

        name = "lbr-plutus";

        inherit (self.config.settings.haskell) index-state compiler-nix-name;

        extraHackage = [ "${config.packages.lbr-prelude-haskell-src}" ];

        modules = [
          (_: {
            packages = {
              allComponent.doHoogle = true;
              allComponent.doHaddock = true;

              # Enable strict compilation
              lbr-plutus.configureFlags = [ "-f-dev" ];
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
        inputs.mlabs-tooling.lib.moduleMod
        project
      ]).flake { };

    in

    {
      devShells.dev-lbr-plutus-haskell = hsNixFlake.devShell;

      packages = {

        lbr-plutus-haskell-src = pkgs.stdenv.mkDerivation {
          name = "lbr-plutus-haskell-src";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

        lbr-plutus-haskell-lib = hsNixFlake.packages."lbr-plutus:lib:lbr-plutus";
        lbr-plutus-haskell-tests = hsNixFlake.packages."lbr-plutus:test:tests";
      };

      inherit (hsNixFlake) checks;

    };
}
