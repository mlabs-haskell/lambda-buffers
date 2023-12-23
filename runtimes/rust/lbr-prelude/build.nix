{ inputs, ... }: {
  perSystem = { config, ... }:
    let
      rustFlake =
        config.lbf-nix.rustFlake {
          src = ./.;
          inherit (inputs) crane;
          crateName = "lbr-prelude";
          extraSources = [
            {
              name = "lbr-prelude-derive";
              path = config.packages.lbr-prelude-derive-rust-src;
            }
          ];
          devShellHook = config.settings.shell.hook;
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
