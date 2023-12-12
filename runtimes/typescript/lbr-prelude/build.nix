{ ... }:
{
  perSystem = { config, ... }:
    let
      typescriptFlake =
        config.lbf-nix.typescriptFlake {
          name = "lbr-prelude";
          src = ./.;

          devShellTools = config.settings.shell.tools;
          devShellHook = config.settings.shell.hook;
        };
    in
    {
      inherit (typescriptFlake) packages checks devShells;
    };

}
