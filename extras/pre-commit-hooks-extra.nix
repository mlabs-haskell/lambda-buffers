{ inputs, ... }: {
  imports = [
    inputs.pre-commit-hooks.flakeModule # Adds perSystem.pre-commit options
  ];
  perSystem = { pkgs, ... }:
    let
      rustfmt = pkgs.rust-bin.stable.latest.rustfmt;
      deno = pkgs.deno;
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

        denofmt =
          {
            name = "denofmt";
            description = "Format Typescript code.";
            entry = "${deno}/bin/deno fmt";
            files = "(\\.ts$)|(^tsconfig?(-base)\\.json$)";
          };

        denolint =
          {
            name = "denolint";
            description = "Lint Typescript code.";
            entry = "${deno}/bin/deno lint";
            files = "\\.ts$";
          };

      };
    };
}
