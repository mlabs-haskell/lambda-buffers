{ inputs, ... }:
{
  perSystem =
    { config, system, ... }:

    let
      rustFlake = inputs.flake-lang.lib.${system}.rustFlake {
        src = ./.;
        crateName = "lbt-plutus";

        generateDocs = false;
        extraSources = [
          config.packages.lbf-plutus-golden-api-rust
          config.packages.lbf-prelude-rust
          config.packages.lbf-plutus-rust
        ];
        data = [
          {
            name = "lbt-plutus-golden-data";
            path = config.packages.lbt-plutus-golden-rust;
          }
        ];
        devShellHook = config.settings.shell.hook;

      };
    in
    {

      inherit (rustFlake) packages checks devShells;

    };
}
