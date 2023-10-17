_: {
  perSystem = { config, ... }:
    {
      packages.lbf-prelude-golden-api-haskell = config.overlayAttrs.lbf-nix.lbfPreludeHaskell {
        name = "lbf-prelude-golden-api";
        src = ./.;
        files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
      };

      packages.lbf-prelude-golden-api-purescript = config.overlayAttrs.lbf-nix.lbfPreludePurescript {
        name = "lbf-prelude-golden-api";
        src = ./.;
        files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
      };

    };
}

