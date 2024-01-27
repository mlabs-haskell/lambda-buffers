{ config, inputs, flake-parts-lib, lib, ... }: {

  # Makes a system agnostic option (dunno why I needed this).
  options.lbf-nix = lib.mkOption {
    type = lib.types.anything; # probably not the best type
    default = { };
  };

  # Makes it available in the system agnostic `lib` argument.
  config = {
    _module.args.lib = config.flake.lib // {
      inherit (config) lbf-nix;
    };

    # Sets the above set option to system ones.
    lbf-nix = lib.genAttrs config.systems (system: (config.perSystem system).lbf-nix);

    # Makes `lib.x86_64-linux.xyz` available
    flake.lib = config.lbf-nix;
  };

  options = {

    # Makes a per system `lbf-nix` option.
    perSystem = flake-parts-lib.mkPerSystemOption
      ({ pkgs, config, pkgsForCtl, pkgsForHaskellNix, pkgsForRust, ... }: {

        options.lbf-nix = lib.mkOption {
          type = lib.types.anything;
          default = { };
        };

        # Sets a per system `lbf-nix` option.
        config = {
          devShells.dev-nix = pkgs.mkShell {
            name = "dev-nix";
            shellHook = config.settings.shell.hook;
            buildInputs = config.settings.shell.tools;
          };

          lbf-nix = {
            # NOTE(bladyjoker): If you need to add a function the export externally and use internally via config.lbf-nix, add it here.

            purescriptFlake = import ./flake-purescript.nix pkgsForCtl;

            rustFlake = import ./flake-rust.nix pkgsForRust;
            haskellData = import ./haskell-data.nix pkgs;
            haskellFlake = import ./flake-haskell.nix pkgsForHaskellNix;
            haskellPlutusFlake = import ./flake-haskell-plutus.nix inputs.cardano-haskell-packages pkgsForHaskellNix;
          };

          # Makes it available in the per system `lib` argument.
          _module.args.lib = lib // {
            inherit (config) lbf-nix;
          };

        };

      });

  };
}
