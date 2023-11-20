{ ... }:
{
  perSystem = { config, ... }:
    let
      typescriptFlake =
        config.overlayAttrs.extras.typescriptFlake {
          name = "lbr-prelude-typescript";
          src = ./.;
        };
    in
    {
      inherit (typescriptFlake) packages checks devShells;
    };

}
