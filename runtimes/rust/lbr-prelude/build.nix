{ inputs, ... }: {
  perSystem = { system, config, ... }:
    let
      rustFlake =
        config.overlayAttrs.extras.rustFlake {
          inherit system;
          src = ./.;
          crane = inputs.crane;
          crateName = "lbr-prelude";
          localDeps = [
            {
              name = "lbr-prelude-derive";
              path = ../lbr-prelude-derive;
            }
          ];
          dataDeps = [ ];
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
