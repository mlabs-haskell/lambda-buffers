{ inputs, ... }:
{
  perSystem = { config, system, ... }:
    let
      tsFlake =
        inputs.flake-lang.lib.${system}.typescriptFlake {
          name = "lbt-prelude";
          src = ./.;
          npmExtraDependencies = [
            config.packages.lbf-prelude-golden-api-typescript
          ];

          devShellTools = config.settings.shell.tools;
          devShellHook = config.settings.shell.hook;

          data =
            [
              {
                name = "lbt-prelude-golden-data";
                path = config.packages.lbt-prelude-golden-typescript;
              }
            ];
        };
    in
    {
      packages = {
        inherit (tsFlake.packages) lbt-prelude-typescript-tgz;
      };

      inherit (tsFlake) devShells checks;
    };
}
