{ inputs, ... }: {
  perSystem = { config, system, ... }:

    let
      rustFlake = inputs.flake-lang.lib.${system}.rustFlake {
        src = ./.;
        inherit (inputs) crane;
        crateName = "lbt-plutus";

        extraSources = [
          {
            name = "lbf-plutus-rust-golden-api";
            path = config.packages.lbf-plutus-golden-api-rust;
          }
          {
            name = "lbf-prelude";
            path = config.packages.lbf-prelude-rust;
          }
          {
            name = "lbf-plutus";
            path = config.packages.lbf-plutus-rust;
          }
          {
            name = "lbr-prelude";
            path = config.packages.lbr-prelude-rust-src;
          }
          {
            name = "lbr-prelude-derive";
            path = config.packages.lbr-prelude-derive-rust-src;
          }
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
