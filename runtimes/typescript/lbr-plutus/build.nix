{ ... }:
{
  perSystem = { config, ... }:
    let
      typescriptFlake =
        config.lbf-nix.typescriptFlake {
          name = "lbr-plutus";
          src = ./.;
          npmDependencies = [ config.packages."lbr-prelude-typescript-tgz" ];

          devShellTools = config.settings.shell.tools;
          devShellHook = config.settings.shell.hook;

          npmDepsHash = "sha256-0FtFC2ftEjB3iSVBBpUi2m5ulEGL25g+w1GIsB7s7GU=";
        };
    in
    {
      inherit (typescriptFlake) packages checks devShells;

      packages = {
        lbr-plutus-typescript = typescriptFlake.packages.lbr-plutus-typescript;
        lbr-plutus-typescript-tgz = typescriptFlake.packages.lbr-plutus-typescript-tgz;
        lbr-plutus-typescript-nix-npm-folder-dependencies = typescriptFlake.packages.lbr-plutus-typescript-nix-npm-folder-dependencies;
        lbr-plutus-typescript-node2nix = typescriptFlake.packages.lbr-plutus-typescript-node2nix;
      };

      inherit (typescriptFlake) checks devShells;
    };
}
