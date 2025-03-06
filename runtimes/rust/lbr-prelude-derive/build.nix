{ inputs, ... }:
{
  perSystem =
    { config, system, ... }:
    let
      rustFlake = inputs.flake-lang.lib.${system}.rustFlake {
        src = ./.;
        crateName = "lbr-prelude-derive";
        extraSources = [
          config.packages.lbr-prelude-rust-src
        ];
        devShellHook = config.settings.shell.hook;
      };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
