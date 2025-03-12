{ inputs, ... }:
{
  perSystem =
    {
      inputs',
      config,
      system,
      ...
    }:
    let
      typescriptFlake = inputs.flake-lang.lib.${system}.typescriptFlake {
        name = "lbr-plutus";
        src = ./.;
        npmExtraDependencies = [
          inputs'.plutus-ledger-api-typescript.packages.lib
          config.packages."lbr-prelude-typescript-lib"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;

      };
    in
    {
      packages = {
        inherit (typescriptFlake.packages)
          lbr-plutus-typescript
          lbr-plutus-typescript-lib
          lbr-plutus-typescript-tgz
          lbr-plutus-typescript-node2nix
          ;
      };

      inherit (typescriptFlake) checks devShells;
    };
}
