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
          name = "prelude-sample-project";
          src = ./.;
          npmDependencies = [
            mySchema.packages.myschema-lb-typescript-tgz
            inputs'.prelude-typescript.packages.tgz
            config.packages.lbr-prelude-typescript-tgz
            config.packages.lbf-prelude-typescript
          ];

          devShellTools = config.settings.shell.tools;
          devShellHook = config.settings.shell.hook;
        };
    in
    {

      packages = {
        prelude-sample-project-typescript = typescriptFlake.packages.prelude-sample-project-typescript;
        # prelude-sample-project-typescript-tgz = typescriptFlake.packages.prelude-sample-project-typescript-tgz;
        prelude-sample-project-typescript-lbf-my-schema = mySchema.packages.myschema-lb-typescript;
      };

      devShells = {
        prelude-sample-project-typescript = typescriptFlake.devShells.prelude-sample-project-typescript;
      };

      checks = {
        prelude-sample-project-typescript-test = typescriptFlake.checks.prelude-sample-project-typescript-test;
      };
    };
}
