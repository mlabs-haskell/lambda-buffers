{ ... }:
{
  perSystem = { system, config, ... }:
    let
      typescriptFlake =
        config.overlayAttrs.extras.typescriptFlake {
          name = "lbr-prelude-typescript";
          src = ./.;
          inherit system;
        };
    in
    {
      inherit (typescriptFlake) packages checks devShells;
    };

}
