{ inputs, ... }: {
  perSystem = { config, ... }:
    let
      rustFlake =
        config.lbf-nix.rustFlake {
          src = ./.;
          crane = inputs.crane;
          crateName = "lbr-prelude";
          extraSources = [
            {
              name = "lbr-prelude-derive";
              path = ../lbr-prelude-derive;
            }
          ];
          devShellHook = config.settings.shell.hook;
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
