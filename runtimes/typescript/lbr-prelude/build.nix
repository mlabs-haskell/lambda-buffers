{ inputs, ... }:
{
  perSystem = { system, inputs', config, ... }:
    let
      typescriptFlake =
        inputs.flake-lang.lib.${system}.typescriptFlake {
          name = "lbr-prelude";
          src = ./.;

          devShellTools = config.settings.shell.tools;
          devShellHook = config.settings.shell.hook;

          npmExtraDependencies = [ inputs'.prelude-typescript.packages.tgz ];
        };
    in
    {
      packages = {
        inherit (typescriptFlake.packages)
          lbr-prelude-typescript
          lbr-prelude-typescript-tgz
          lbr-prelude-typescript-node2nix;
      };

      inherit (typescriptFlake) checks devShells;
    };

}
