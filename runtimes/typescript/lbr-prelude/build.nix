{ ... }:
{
  perSystem = { config, ... }:
    let
      typescriptFlake =
        config.overlayAttrs.extras.typescriptFlake {
          name = "lbr-prelude";
          src = ./.;
        };
    in
    {
      inherit (typescriptFlake) packages checks devShells;
    };

}
