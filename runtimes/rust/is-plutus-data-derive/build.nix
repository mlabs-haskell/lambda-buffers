{ inputs, ... }: {
  perSystem = { config, system, ... }:
    let
      rustFlake =
        inputs.flake-lang.lib.${system}.rustFlake {
          src = ./.;
          version = "0";
          crateName = "is-plutus-data-derive";
          devShellHook = config.settings.shell.hook;
          cargoNextestExtraArgs = "--all-features";
          generateDocs = false;
          runTests = false;
        };

    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
