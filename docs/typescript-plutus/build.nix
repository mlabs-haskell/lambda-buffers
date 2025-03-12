{ inputs, ... }:
{
  imports = [ ./api/lbf/build.nix ];

  perSystem =
    { config, system, ... }:
    let
      tsFlake = inputs.flake-lang.lib.${system}.typescriptFlake {
        name = "plutus-sample-project";
        src = ./.;

        npmExtraDependencies = [
          config.packages.lbf-plutus-sample-project-typescript
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };
    in
    {
      packages = {
        inherit (tsFlake.packages) plutus-sample-project-typescript plutus-sample-project-typescript-tgz;
      };

      inherit (tsFlake) devShells checks;
    };
}
