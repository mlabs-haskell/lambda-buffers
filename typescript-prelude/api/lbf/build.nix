_:
{
  perSystem = { config, ... }:
    let
      tsFlake =
        config.lbf-nix.lbfPreludeTypescript {
          name = "lbf-prelude-sample-project";
          src = ./.;
          files = [ "MySchema.lbf" ];
        };
    in
    {
      packages = {
        inherit (tsFlake.packages) lbf-prelude-sample-project-typescript lbf-prelude-sample-project-typescript-tgz;
      };
    };
}
