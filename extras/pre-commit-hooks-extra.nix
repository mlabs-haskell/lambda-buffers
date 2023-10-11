{ inputs, ... }: {
  imports = [
    inputs.pre-commit-hooks.flakeModule # Adds perSystem.pre-commit options
  ];
  perSystem = { pkgs, lib, config, ... }:
    let
      inherit (config.pre-commit.settings.rawConfig.rust) cargoCratePaths;
      tools = pkgs;
      defClippySettings = { denyWarnings = false; offline = true; };
      defSettings = { clippy = defClippySettings; };
      settings = lib.recursiveUpdate defSettings config.pre-commit.settings;

    in
    {
      pre-commit.settings.hooks = {
        rustfmt-monorepo =
          let
            wrapper = pkgs.symlinkJoin {
              name = "rustfmt-wrapped";
              paths = [ tools.rustfmt ];
              nativeBuildInputs = [ pkgs.makeWrapper ];
              postBuild = ''
                wrapProgram $out/bin/cargo-fmt \
                  --prefix PATH : ${lib.makeBinPath [ tools.cargo tools.rustfmt ]}
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
        clippy-monorepo =
          let
            wrapper = pkgs.symlinkJoin {
              name = "clippy-wrapped";
              paths = [ tools.clippy ];
              nativeBuildInputs = [ pkgs.makeWrapper ];
              postBuild = ''
                wrapProgram $out/bin/cargo-clippy \
                  --prefix PATH : ${lib.makeBinPath [ tools.cargo ]}
              '';
            };
          in
          {
            name = "clippy";
            description = "Lint Rust code.";
            entry =
              builtins.concatStringsSep " && "
                (builtins.map
                  (path:
                    "${wrapper}/bin/cargo-clippy clippy --manifest-path '${path}/Cargo.toml' ${lib.optionalString settings.clippy.offline "--offline"} -- ${lib.optionalString settings.clippy.denyWarnings "-D warnings"}")
                  cargoCratePaths);

            files = "\\.rs$";
            pass_filenames = false;
          };
      };
    };
}
