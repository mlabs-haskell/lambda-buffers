{ inputs, ... }: {
  perSystem = { system, config, ... }:
    let
      rustFlake =
        config.overlayAttrs.extras.rustFlake {
          inherit system;
          src = ./.;
          crane = inputs.crane;
          crateName = "lbr-prelude-derive";
          localDeps = [
            {
              name = "lbr-prelude";
              path = ../lbr-prelude;
            }
          ];
          dataDeps = [ ];
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
