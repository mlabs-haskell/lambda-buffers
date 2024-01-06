{ ... }:
{
  perSystem = { inputs', config, ... }:
    let
      mySchema = config.lbf-nix.lbfPreludeTypescript {
        name = "myschema-lb";
        src = ./lbf;
        files = [ "MySchema.lbf" ];
      };

      typescriptFlake =
        config.lbf-nix.typescriptFlake {
          name = "plutus-sample-project";
          src = ./.;
          npmDependencies = [
            mySchema.packages.myschema-lb-typescript-tgz

            inputs'.prelude-typescript.packages.tgz
            config.packages.lbr-prelude-typescript-tgz
            config.packages.lbf-prelude-typescript

            inputs'.plutus-ledger-api-typescript.packages.tgz
            config.packages.lbr-plutus-typescript-tgz
            config.packages.lbf-plutus-typescript
          ];

          devShellTools = config.settings.shell.tools;
          devShellHook = config.settings.shell.hook;
        };
    in
    {

      packages = {
        plutus-sample-project-typescript = typescriptFlake.packages.plutus-sample-project-typescript;
        # plutus-sample-project-typescript-tgz = typescriptFlake.packages.plutus-sample-project-typescript-tgz;
        plutus-sample-project-typescript-lbf-my-schema = mySchema.packages.myschema-lb-typescript;
      };

      devShells = {
        plutus-sample-project-typescript = typescriptFlake.devShells.plutus-sample-project-typescript;
      };

      checks = {
        plutus-sample-project-typescript-test = typescriptFlake.checks.plutus-sample-project-typescript-test;
      };
    };
}
