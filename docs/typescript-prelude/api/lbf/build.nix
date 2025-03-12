_: {
  perSystem =
    { config, ... }:
    let
      lbf-prelude-sample-project-typescript = config.lbf-nix.lbfPreludeTypescript {
        name = "lbf-prelude-sample-project";
        src = ./.;
        files = [ "MySchema.lbf" ];
      };
    in
    {
      packages = {
        inherit lbf-prelude-sample-project-typescript;
      };
    };
}
