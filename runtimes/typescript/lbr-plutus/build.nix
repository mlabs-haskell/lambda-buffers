{ ... }:
{
  perSystem = { config, ... }:
    let
      typescriptFlake =
        config.overlayAttrs.extras.typescriptFlake {
          name = "lbr-plutus-typescript";
          src = ./.;
          dependencies = [ config.packages."lbr-prelude-typescript-tarball" ];
        };
    in
    {
      inherit (typescriptFlake) packages checks devShells;
    };
}
