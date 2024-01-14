{ inputs, ... }:
{
  perSystem = { config, system, ... }:
    let
      mySchema = config.lbf-nix.lbfPlutusTypescript {
        name = "myschema-lb";
        src = ./lbf;
        files = [ "MySchema.lbf" ];
      };

      tsFlake =
        inputs.flake-lang.lib.${system}.typescriptFlake {
          name = "plutus-sample-project";
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
        inherit (tsFlake.packages) plutus-sample-project-typescript plutus-sample-project-typescript-tgz;
        plutus-sample-project-typescript-lbf-my-schema = mySchema.packages.myschema-lb-typescript;
      };

      inherit (tsFlake) devShells checks;
    };
}
