{ inputs, ... }:
{
  imports = [ ./api/lbf/build.nix ];

  perSystem =
    { config, system, ... }:
    let
      tsFlake = inputs.flake-lang.lib.${system}.typescriptFlake {
        name = "prelude-sample-project";
        src = ./.;
        npmExtraDependencies = [
          config.packages.lbf-prelude-sample-project-typescript
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };
    in
    {
      packages = {
        inherit (tsFlake.packages) prelude-sample-project-typescript prelude-sample-project-typescript-tgz;
      };

      inherit (tsFlake) devShells checks;
    };
}
