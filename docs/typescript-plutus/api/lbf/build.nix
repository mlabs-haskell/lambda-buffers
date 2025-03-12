_: {
  perSystem =
    { config, ... }:
    let
      lbf-plutus-sample-project-typescript = config.lbf-nix.lbfPlutusTypescript {
        name = "lbf-plutus-sample-project";
        src = ./.;
        files = [ "MySchema.lbf" ];
      };
    in
    {
      packages = {
        inherit lbf-plutus-sample-project-typescript;
      };
    };
}
