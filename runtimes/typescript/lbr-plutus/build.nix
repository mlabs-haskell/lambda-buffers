_:
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

      packages = {
        inherit (typescriptFlake.packages) lbr-plutus-typescript lbr-plutus-typescript-tgz lbr-plutus-typescript-node2nix;
      };

      devShells = {
        inherit (typescriptFlake.devShells) lbr-plutus-typescript;
      };

      checks = {
        inherit (typescriptFlake.checks) lbr-plutus-typescript-test;
      };

      # TODO(jaredponn): for some reason, writing the more terse `inherit`
      # seems to cause infinite recursion.
      # ```
      # inherit (typescriptFlake) packages checks devShells;
      # ```
      # So instead, we explicitly write out all the attribute names and what
      # they are assigned to.
    };
}
