{ ... }:
{
  perSystem = { config, ... }:
    let
      typescriptFlake =
        config.lbf-nix.typescriptFlake {
          name = "lbr-prelude";
          src = ./.;
        };
    in
    {
      packages = {
        lbr-prelude-typescript = typescriptFlake.packages.lbr-prelude-typescript;
        lbr-prelude-typescript-tgz = typescriptFlake.packages.lbr-prelude-typescript-tgz;
        lbr-prelude-typescript-node2nix = typescriptFlake.packages.lbr-prelude-typescript-node2nix;
      };

      devShells =
        {
          lbr-prelude-typescript = typescriptFlake.devShells.lbr-prelude-typescript;
        };

      checks = {
        lbr-prelude-typescript-test = typescriptFlake.checks.lbr-prelude-typescript-test;
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
