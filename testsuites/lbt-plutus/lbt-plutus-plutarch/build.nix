{ inputs, ... }:
{
  perSystem = { pkgs, config, ... }:
    let
      project = { lib, ... }: {
        src = ./.;

        name = "lbt-plutus-plutarch";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        extraHackage = [
          # Load Plutarch Haskell support
          "${config.packages.lbf-prelude-plutarch}"
          "${config.packages.lbf-plutus-plutarch}"
          "${config.packages.lbr-plutarch-src}"
          # Load pure Haskell support
          "${config.packages.lbf-prelude-haskell}"
          "${config.packages.lbf-plutus-haskell}"
          "${config.packages.lbr-prelude-haskell-src}"
          "${config.packages.lbr-plutus-haskell-src}"
          # Golden api
          "${config.packages.lbf-plutus-golden-api-plutarch}"
          "${config.packages.lbf-plutus-golden-api-haskell}"
          # Golden data
          "${config.packages.lbt-plutus-golden-haskell}"
          # Plutarch itself
          "${inputs.plutarch}"
          "${inputs.plutarch}/plutarch-extra"
        ];

        modules = [
          (_: {
            packages = {
              allComponent.doHoogle = true;
              allComponent.doHaddock = true;

              # Enable strict compilation
              lbt-plutus-plutarch.configureFlags = [ "-f-dev" ];
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
      devShells.dev-lbt-plutus-plutarch = hsNixFlake.devShell;

      packages = {
        lbt-plutus-plutarch-lib = hsNixFlake.packages."lbt-plutus-plutarch:lib:lbt-plutus-plutarch";
        lbt-plutus-plutarch-tests = hsNixFlake.packages."lbt-plutus-plutarch:test:tests";
      };

      checks.check-lbt-plutus-plutarch = hsNixFlake.checks."lbt-plutus-plutarch:test:tests";
    };
}
