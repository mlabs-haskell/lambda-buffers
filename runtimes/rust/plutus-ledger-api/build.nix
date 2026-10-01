{ inputs, lib, ... }: {
  perSystem = { config, system, pkgs, ... }:
    let
      rustFlake =
        inputs.flake-lang.lib.${system}.rustFlake {
          src = ./.;
          version = "3";
          crateName = "plutus-ledger-api";
          devShellHook = config.settings.shell.hook;
          cargoNextestExtraArgs = "--all-features";
          extraSourceFilters = [
            (path: _type: builtins.match ".*golden$" path != null)
          ];
          extraSources = [
            config.packages.is-plutus-data-derive-rust-src
            config.packages.lbr-prelude-rust-src
            config.packages.lbr-prelude-derive-rust-src
          ];
        };

      devShellHook = config.settings.shell.hook;
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
