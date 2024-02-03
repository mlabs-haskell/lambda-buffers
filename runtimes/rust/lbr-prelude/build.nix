{ inputs, ... }: {
  perSystem = { config, system, ... }:
    let
      rustFlake =
        inputs.flake-lang.lib.${system}.rustFlake {
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
