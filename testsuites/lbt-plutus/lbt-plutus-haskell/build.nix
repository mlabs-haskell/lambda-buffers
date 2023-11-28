_:
{
  perSystem = { config, ... }:
    let
      hsFlake = config.lbf-nix.haskellPlutusFlake {
        src = ./.;

        name = "lbt-plutus-haskell";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          "${config.packages.lbr-prelude-haskell-src}"
          "${config.packages.lbf-prelude-haskell}"
          "${config.packages.lbr-plutus-haskell-src}"
          "${config.packages.lbf-plutus-haskell}"
          "${config.packages.lbf-plutus-golden-api-haskell}"
          "${config.packages.lbt-plutus-golden-haskell}"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in

    {
      devShells.dev-lbt-plutus-haskell = hsFlake.devShell;

      packages = {
        lbt-plutus-haskell-lib = hsFlake.packages."lbt-plutus-haskell:lib:lbt-plutus-haskell";
        lbt-plutus-haskell-golden-cli = hsFlake.packages."lbt-plutus-haskell:exe:lbt-plutus-golden";
        lbt-plutus-haskell-tests = hsFlake.packages."lbt-plutus-haskell:test:tests";
      };

      checks.check-lbt-plutus-haskell = hsFlake.checks."lbt-plutus-haskell:test:tests";
    };
}
