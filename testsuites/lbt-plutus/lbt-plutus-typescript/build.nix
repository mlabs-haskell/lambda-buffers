{ inputs, ... }:
{
  perSystem =
    { config, system, ... }:
    let
      tsFlake = inputs.flake-lang.lib.${system}.typescriptFlake {
        name = "lbt-plutus";
        src = ./.;
        npmExtraDependencies = [
          config.packages.lbf-plutus-golden-api-typescript
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;

        data = [
          {
            name = "lbt-plutus-golden-data";
            path = config.packages.lbt-plutus-golden-typescript;
          }
        ];
      };
    in
    {
      inherit (tsFlake) devShells checks;
    };
}
