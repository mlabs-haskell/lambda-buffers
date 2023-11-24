{ ... }:
{
  perSystem = { config, ... }:
    let
      typescriptFlake =
        config.overlayAttrs.extras.typescriptFlake {
          name = "lbr-plutus";
          src = ./.;
          dependencies = [ config.packages."lbr-prelude-typescript-tgz" ];
        };
    in
    {
      inherit (typescriptFlake) packages checks devShells;
    };
}
