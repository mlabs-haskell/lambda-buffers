{ inputs, ... }: {
  imports = [
    inputs.pre-commit-hooks.flakeModule # Adds perSystem.pre-commit options
  ];
  perSystem = { pkgs, ... }:
    let
      inherit (pkgs.rust-bin.stable.latest) rustfmt;
      inherit (pkgs) deno;
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

        # TODO(jaredponn): Why do we use our strange version of `denofmt` and
        # `denolint`? The default implemented version in `pre-commit-hooks.nix`
        # is a bit buggy (see
        # https://github.com/cachix/pre-commit-hooks.nix/issues/374), and the
        # latest version of `deno` on nix doesn't allow explicitly applying
        # the formatter to specific files
        my-denofmt =
          {
            name = "denofmt";
            description = "Format Typescript code.";
            entry = "${deno}/bin/deno fmt";
            files = "(\\.m?ts$)|(^tsconfig?(-base)\\.json$)";
          };

        my-denolint =
          {
            name = "denolint";
            description = "Lint Typescript code.";
            entry = "${deno}/bin/deno lint";
            files = "\\.m?ts$";
          };

      };
    };
}
