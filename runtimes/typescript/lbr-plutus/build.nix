{ ... }:
{
  perSystem = { system, config, ... }:
    let
      typescriptFlake =
        config.overlayAttrs.extras.typescriptFlake {
          name = "lbr-plutus-typescript";
          src = ./.;
          inherit system;
          dependencies = [ config.packages."lbr-prelude-typescript-tarball" ];
        };
    in
    {
      inherit (typescriptFlake) packages checks devShells;
    };
}
