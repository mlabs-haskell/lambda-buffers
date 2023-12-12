{ inputs, ... }: {
  perSystem = { config, ... }:
    let
      rustFlake =
        config.lbf-nix.rustFlake {
          src = ./.;
          crane = inputs.crane;
          crateName = "lbr-prelude-derive";
          extraSources = [
            {
              name = "lbr-prelude";
              path = config.packages.lbr-prelude-rust-src;
            }
          ];
          devShellHook = config.settings.shell.hook;
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
