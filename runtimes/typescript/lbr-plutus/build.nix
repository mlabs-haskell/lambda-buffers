{ ... }:
{
  perSystem = { config, ... }:
    let
      typescriptFlake =
        config.lbf-nix.typescriptFlake {
          name = "lbr-plutus";
          src = ./.;
          dependencies = [ config.packages."lbr-prelude-typescript-tgz" ];
        };
    in
    {

      packages = {
        lbr-plutus-typescript = typescriptFlake.packages.lbr-plutus-typescript;
        lbr-plutus-typescript-tgz = typescriptFlake.packages.lbr-plutus-typescript-tgz;
        lbr-plutus-typescript-node2nix = typescriptFlake.packages.lbr-plutus-typescript-node2nix;
      };

      devShells = {
        lbr-plutus-typescript = typescriptFlake.devShells.lbr-plutus-typescript;
      };

      checks = {
        lbr-plutus-typescript-test = typescriptFlake.checks.lbr-plutus-typescript-test;
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
