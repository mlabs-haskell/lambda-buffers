{ inputs, ... }: {
  perSystem = { config, system, ... }:

    let
      rustFlake = inputs.flake-lang.lib.${system}.rustFlake {
        src = ./.;
        inherit (inputs) crane;
        crateName = "lbt-prelude";

        extraSources = [
          config.packages.lbf-prelude-golden-api-rust
          config.packages.lbf-prelude-rust
          config.packages.lbr-prelude-rust-src
          config.packages.lbr-prelude-derive-rust-src
        ];
        data = [
          {
            name = "lbt-prelude-golden-data";
            path = config.packages.lbt-prelude-golden-rust;
          }
        ];
        devShellHook = config.settings.shell.hook;

      };
    in
    {

      inherit (rustFlake) packages checks devShells;

    };
}
