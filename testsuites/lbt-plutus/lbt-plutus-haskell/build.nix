{ inputs, ... }:
{
  perSystem = { pkgs, config, ... }:
    let
      project = { lib, ... }: {
        src = ./.;

        name = "lbt-plutus-haskell";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        extraHackage = [
          "${config.packages.lbr-prelude-haskell-src}"
          "${config.packages.lbf-prelude-haskell}"
          "${config.packages.lbr-plutus-haskell-src}"
          "${config.packages.lbf-plutus-haskell}"
          "${config.packages.lbf-plutus-golden-api-haskell}"
          "${config.packages.lbt-plutus-golden-haskell}"
        ];

        modules = [
          (_: {
            packages = {
              allComponent.doHoogle = true;
              allComponent.doHaddock = true;

              # Enable strict compilation
              lbt-plutus-haskell.configureFlags = [ "-f-dev" ];
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
      devShells.dev-lbt-plutus-haskell = hsNixFlake.devShell;

      packages = {
        lbt-plutus-haskell-lib = hsNixFlake.packages."lbt-plutus-haskell:lib:lbt-plutus-haskell";
        lbt-plutus-haskell-golden-cli = hsNixFlake.packages."lbt-plutus-haskell:exe:lbt-plutus-golden";
        lbt-plutus-haskell-tests = hsNixFlake.packages."lbt-plutus-haskell:test:tests";
      };

      checks.check-lbt-plutus-haskell = hsNixFlake.checks."lbt-plutus-haskell:test:tests";
    };
}
