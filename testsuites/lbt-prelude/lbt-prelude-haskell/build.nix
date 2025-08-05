{ inputs, ... }:
{
  perSystem =
    { config, system, ... }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellFlake {
        src = ./.;

        name = "lbt-prelude-haskell";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          "${config.packages.lbr-prelude-haskell-src}"
          "${config.packages.lbf-prelude-haskell}"
          "${config.packages.lbf-prelude-golden-api-haskell}"
          "${config.packages.lbt-prelude-golden-haskell}"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in
    {
      devShells.dev-lbt-prelude-haskell = hsFlake.devShells.default;

      packages = {
        lbt-prelude-haskell-lib = hsFlake.packages."lbt-prelude-haskell:lib:lbt-prelude-haskell";
        lbt-prelude-haskell-golden-cli = hsFlake.packages."lbt-prelude-haskell:exe:lbt-prelude-golden";
        lbt-prelude-haskell-tests = hsFlake.packages."lbt-prelude-haskell:test:tests";
      };

      checks.check-lbt-prelude-haskell = hsFlake.checks."lbt-prelude-haskell:test:tests";
    };
}
