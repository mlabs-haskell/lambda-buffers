_:
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
      packages = {
        inherit (typescriptFlake.packages) lbr-prelude-typescript lbr-prelude-typescript-tgz lbr-prelude-typescript-node2nix;
      };

      devShells =
        {
          inherit (typescriptFlake.devShells) lbr-prelude-typescript;
        };

      checks = {
        inherit (typescriptFlake.checks) lbr-prelude-typescript-test;
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
