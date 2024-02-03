{ inputs, ... }: {
  perSystem = { config, system, ... }:

    let
      rustFlake = inputs.flake-lang.lib.${system}.rustFlake {
        src = ./.;
        inherit (inputs) crane;
        crateName = "lbt-plutus";

        extraSources = [
          config.packages.lbf-plutus-golden-api-rust
          config.packages.lbf-prelude-rust
          config.packages.lbf-plutus-rust
          config.packages.lbr-prelude-rust-src
          config.packages.lbr-prelude-derive-rust-src
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
