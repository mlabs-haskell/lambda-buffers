_:
{
  perSystem = { config, ... }:
    let
      tsFlake =
        config.lbf-nix.lbfPlutusTypescript {
          name = "lbf-plutus-sample-project";
          src = ./.;
          files = [ "MySchema.lbf" ];
        };
    in
    {
      packages = {
        inherit (tsFlake.packages) lbf-plutus-sample-project-typescript lbf-plutus-sample-project-typescript-tgz;
      };
    };
}
