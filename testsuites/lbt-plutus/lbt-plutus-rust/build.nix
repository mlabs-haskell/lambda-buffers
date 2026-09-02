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
          # Local plutus-ledger-api checkout, wired in via `[patch.crates-io]` in
          # Cargo.toml so every crate in the graph (including the generated lbf-*
          # crates) resolves to it instead of the crates.io release.
          inputs.plutus-ledger-api-rust.packages.${system}.plutus-ledger-api-rust-src
          inputs.plutus-ledger-api-rust.packages.${system}.is-plutus-data-derive-rust-src
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
