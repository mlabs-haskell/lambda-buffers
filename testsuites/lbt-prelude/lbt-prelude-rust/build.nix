{ inputs, ... }: {
  perSystem = { config, ... }:

    let
      rustFlake = config.lbf-nix.rustFlake {
        src = ./.;
        crane = inputs.crane;
        crateName = "lbt-prelude";

        extraSources = [
          {
            name = "lbf-prelude-golden-api";
            path = config.packages.lbf-prelude-golden-api-rust;

          }
          {
            name = "lbf-prelude";
            path = config.packages.lbf-prelude-rust;
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
