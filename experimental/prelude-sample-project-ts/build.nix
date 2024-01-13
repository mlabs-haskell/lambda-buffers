{ inputs, ... }:
{
  perSystem = { config, system, ... }:
    let
      mySchema = config.lbf-nix.lbfPreludeTypescript {
        name = "myschema-lb";
        src = ./lbf;
        files = [ "MySchema.lbf" ];
      };

      tsFlake =
        inputs.flake-lang.lib.${system}.typescriptFlake {
          name = "prelude-sample-project";
          src = ./.;
          npmExtraDependencies = [
            mySchema.packages.myschema-lb-typescript-tgz
          ];

          devShellTools = config.settings.shell.tools;
          devShellHook = config.settings.shell.hook;
        };
    in
    {
      packages = {
        inherit (tsFlake.packages) prelude-sample-project-typescript prelude-sample-project-typescript-tgz;
        prelude-sample-project-typescript-lbf-my-schema = mySchema.packages.myschema-lb-typescript;
      };

      inherit (tsFlake) devShells checks;
    };
}
