{ inputs, ... }:
{
  perSystem = { pkgs, config, ... }:
    let
      project = { lib, ... }: {
        src = ./.;

        name = "lbt-prelude-haskell";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        extraHackage = [
          "${config.packages.lbr-prelude-haskell-src}"
          "${config.packages.lbf-prelude-haskell}"
          "${config.packages.lbf-prelude-golden-api-haskell}"
          "${config.packages.lbt-prelude-golden-haskell}"
        ];

        modules = [
          (_: {
            packages = {
              allComponent.doHoogle = true;
              allComponent.doHaddock = true;

              # Enable strict compilation
              lbt-prelude-haskell.configureFlags = [ "-f-dev" ];
            };
          })
        ];

        shell = {

          withHoogle = true;

          exactDeps = true;

          nativeBuildInputs = config.settings.shell.tools;

          tools = {
            cabal = { };
            haskell-language-server = { };
          };

          shellHook = lib.mkForce config.settings.shell.hook;
        };
      };
      hsNixFlake = (pkgs.haskell-nix.cabalProject' [
        inputs.mlabs-tooling.lib.mkHackageMod
        project
      ]).flake { };
    in

    {
      devShells.dev-lbt-prelude-haskell = hsNixFlake.devShell;

      packages = {
        lbt-prelude-haskell-lib = hsNixFlake.packages."lbt-prelude-haskell:lib:lbt-prelude-haskell";
        lbt-prelude-haskell-golden-cli = hsNixFlake.packages."lbt-prelude-haskell:exe:lbt-prelude-golden";
        lbt-prelude-haskell-tests = hsNixFlake.packages."lbt-prelude-haskell:test:tests";
      };

      checks.check-lbt-prelude-haskell = hsNixFlake.checks."lbt-prelude-haskell:test:tests";
    };
}
