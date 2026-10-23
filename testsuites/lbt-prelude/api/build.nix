_: {
  perSystem =
    { config, ... }:
    {
      packages = {
        lbf-prelude-golden-api-haskell = config.lbf-nix.lbfPreludeHaskell {
          name = "lbf-prelude-golden-api";
          src = ./.;
          files = [
            "Foo.lbf"
            "Foo/Bar.lbf"
            "Days.lbf"
          ];
        };

        lbf-prelude-golden-api-purescript = config.lbf-nix.lbfPreludePurescript {
          name = "lbf-prelude-golden-api";
          src = ./.;
          files = [
            "Foo.lbf"
            "Foo/Bar.lbf"
            "Days.lbf"
          ];
        };

        lbf-prelude-golden-api-rust = config.lbf-nix.lbfPreludeRust {
          extraVersions = config.settings.rust.localCrates;
          name = "lbf-prelude-golden-api";
          src = ./.;
          files = [
            "Foo.lbf"
            "Foo/Bar.lbf"
            "Days.lbf"
          ];
        };

        lbf-prelude-golden-api-typescript = config.lbf-nix.lbfPreludeTypescript {
          name = "lbf-prelude-golden-api";
          src = ./.;
          files = [
            "Foo.lbf"
            "Foo/Bar.lbf"
            "Days.lbf"
          ];
        };
      };
    };
}
