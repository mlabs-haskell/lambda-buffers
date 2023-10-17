{ inputs, ... }: {
  imports = [
    inputs.pre-commit-hooks.flakeModule # Adds perSystem.pre-commit options
  ];
  perSystem = { pkgs, lib, config, ... }:
    let
      inherit (config.pre-commit.settings) rawConfig;
      inherit (rawConfig.rust) cargoCratePaths;
      rust-bin = pkgs.rust-bin.stable.latest;
    in
    {
      pre-commit.settings.hooks = {
        rustfmt-monorepo =
          let
            wrapper = pkgs.symlinkJoin {
              name = "rustfmt-wrapped";
              paths = [ rust-bin.rustfmt ];
              nativeBuildInputs = [ pkgs.makeWrapper ];
              postBuild = ''
                wrapProgram $out/bin/cargo-fmt \
                  --prefix PATH : ${lib.makeBinPath [ rust-bin.cargo rust-bin.rustfmt ]}
              '';
            };
          in
          {
            name = "rustfmt";
            description = "Format Rust code.";
            entry =
              builtins.concatStringsSep " && "
                (builtins.map
                  (path:
                    "${wrapper}/bin/cargo-fmt fmt --manifest-path '${path}/Cargo.toml' -- --color always")

                  cargoCratePaths);
            files = "\\.rs$";
            pass_filenames = false;
          };
      };
    };
}
