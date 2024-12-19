{ inputs, ... }:
{
  perSystem = { config, system, ... }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellPlutusFlake {
        src = ./.;

        name = "plutus-haskell";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          # Haskell native backend
          "${config.packages.lbr-prelude-haskell-src}"
          "${config.packages.lbf-prelude-haskell}"
          "${config.packages.lbr-plutus-haskell-src}"
          "${config.packages.lbf-plutus-haskell}"

          # PlutusTx backend
          "${config.packages.lbr-plutustx-src}"
          "${config.packages.lbf-plutus-plutustx}"
          "${config.packages.lbf-prelude-plutustx}"

          # Plutarch backend
          "${config.packages.lbr-plutarch-src}"
          "${config.packages.lbf-prelude-plutarch}"
          "${config.packages.lbf-plutus-plutarch}"

          # Plutarch itself
          "${inputs.plutarch}"
          "${inputs.plutarch}/plutarch-ledger-api"
        ];

        devShellTools = config.settings.shell.tools ++ [
          config.packages.lbf-prelude-to-haskell
          config.packages.lbf-plutus-to-haskell
          config.packages.lbf-plutus-to-plutarch
          config.packages.lbf-plutus-to-plutustx
        ];

        devShellHook = config.settings.shell.hook;
      };
    in
    {
      # Develop Plutus applications with Haskell, Plutarch and PlutusTx
      devShells.dev-plutus-haskell = hsFlake.devShell;
      packages.play-plutus-haskell-lib = hsFlake.packages."plutus-haskell:lib:plutus-haskell";
    };
}
