{ inputs, lib, ... }:
{
  perSystem = { pkgs, system, inputs', config, ... }:
    {
      devShells.dev-lbt-prelude-golden = config.devShells.dev-pre-commit;

      packages = {
        lbt-prelude-golden-haskell = import ../../../extras/haskell-data.nix {
          inherit pkgs;
          srcs = [ ./. ];
          cabalDataPatterns = [ "**/*.json" ];
          cabalPackageName = "lbt-prelude-golden-data";
        };
      };

    };
}
