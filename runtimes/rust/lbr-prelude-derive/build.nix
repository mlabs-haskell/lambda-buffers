{ inputs, ... }: {
  perSystem = { system, config, ... }:
    let
      rustFlake =
        config.overlayAttrs.extras.rustFlake {
          inherit system;
          src = ./.;
          crane = inputs.crane;
          crateName = "lbr-prelude-derive";
          extraSources = [
            {
              name = "lbr-prelude";
              path = ../lbr-prelude;
            }
          ];
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
