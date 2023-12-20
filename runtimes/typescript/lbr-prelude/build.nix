{ ... }:
{
  perSystem = { inputs', config, ... }:
    let
      typescriptFlake =
        config.lbf-nix.typescriptFlake {
          name = "lbr-prelude";
          src = ./.;

          devShellTools = config.settings.shell.tools;
          devShellHook = config.settings.shell.hook;

          npmDependencies = [ inputs'.prelude-typescript.packages.tgz ];
        };
    in
    {
      packages = {
        lbr-prelude-typescript = typescriptFlake.packages.lbr-prelude-typescript;
        lbr-prelude-typescript-tgz = typescriptFlake.packages.lbr-prelude-typescript-tgz;
        lbr-prelude-typescript-nix-npm-folder-dependencies = typescriptFlake.packages.lbr-prelude-typescript-nix-npm-folder-dependencies;
        lbr-prelude-typescript-node2nix = typescriptFlake.packages.lbr-prelude-typescript-node2nix;
      };

      inherit (typescriptFlake) checks devShells;
    };

}
