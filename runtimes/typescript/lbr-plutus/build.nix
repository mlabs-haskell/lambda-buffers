{ ... }:
{
  perSystem = { config, ... }:
    let
      typescriptFlake =
        config.lbf-nix.typescriptFlake {
          name = "lbr-plutus";
          src = ./.;
          dependencies = [ config.packages."lbr-prelude-typescript-tgz" ];

          devShellTools = config.settings.shell.tools;
          devShellHook = config.settings.shell.hook;
        };
    in
    {
      inherit (typescriptFlake) packages checks devShells;
    };
}
