{ inputs, ... }: {
  perSystem = { config, inputs', system, ... }:
    let
      rustFlake =
        inputs.flake-lang.lib.${system}.rustFlake {
          src = ./.;
          crateName = "lbr-prelude";
          extraSources = [
            config.packages.lbr-prelude-derive-rust-src
            inputs'.plutus-ledger-api-rust.packages.plutus-ledger-api-rust-src
          ];
          devShellHook = config.settings.shell.hook;
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
