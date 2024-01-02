{ ... }:
{
  perSystem = { inputs', config, ... }:
    let
      typescriptFlake =
        config.lbf-nix.typescriptFlake {
          name = "lbr-plutus";
          src = ./.;
          npmDependencies = [
            inputs'.plutus-ledger-api-typescript.packages.tgz
            inputs'.prelude-typescript.packages.tgz
            config.packages."lbr-prelude-typescript-tgz"
          ];

          devShellTools = config.settings.shell.tools;
          devShellHook = config.settings.shell.hook;

        };
    in
    {
      packages = {
        lbr-plutus-typescript = typescriptFlake.packages.lbr-plutus-typescript;
        lbr-plutus-typescript-tgz = typescriptFlake.packages.lbr-plutus-typescript-tgz;
        lbr-plutus-typescript-nix-npm-folder-dependencies = typescriptFlake.packages.lbr-plutus-typescript-nix-npm-folder-dependencies;
        lbr-plutus-typescript-node2nix = typescriptFlake.packages.lbr-plutus-typescript-node2nix;
      };

      inherit (typescriptFlake) checks devShells;
    };
}
