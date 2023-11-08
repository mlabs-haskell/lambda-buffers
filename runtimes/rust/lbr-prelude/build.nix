{ inputs, ... }: {
  perSystem = { system, config, ... }:
    let
      rustFlake =
        config.overlayAttrs.extras.rustFlake {
          inherit system;
          src = ./.;
          crane = inputs.crane;
          crateName = "lbr-prelude";
          extraSources = [
            {
              name = "lbr-prelude-derive";
              path = ../lbr-prelude-derive;
            }
          ];
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
