{ inputs, ... }: {
  perSystem = { config, system, ... }:
    let
      rustFlake =
        inputs.flake-lang.lib.${system}.rustFlake {
          src = ./.;
          inherit (inputs) crane;
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
