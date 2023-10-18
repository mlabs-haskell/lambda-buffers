{ inputs, ... }: {
  imports = [
    inputs.pre-commit-hooks.flakeModule # Adds perSystem.pre-commit options
  ];
  perSystem = { pkgs, ... }:
    let
      rustfmt = pkgs.rust-bin.stable.latest.rustfmt;
    in
    {
      pre-commit.settings.hooks = {
        rustfmt-monorepo =
          {
            name = "rustfmt";
            description = "Format Rust code.";
            entry = "${rustfmt}/bin/rustfmt --color always";
            files = "\\.rs$";
          };
      };
    };
}
