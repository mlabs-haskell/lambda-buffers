{
  config,
  flake-parts-lib,
  lib,
  ...
}:
{

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
    perSystem = flake-parts-lib.mkPerSystemOption (
      { pkgs, config, ... }:
      {

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

          # Makes it available in the per system `lib` argument.
          _module.args.lib = lib // {
            inherit (config) lbf-nix;
          };

        };

      }
    );

  };
}
