_: {
  perSystem = { config, ... }:
    {
      packages.lbf-plutus-golden-api-haskell = config.overlayAttrs.lbf-nix.lbfPlutusHaskell {
        name = "lbf-plutus-golden-api";
        src = ./.;
        files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
      };

      packages.lbf-plutus-golden-api-purescript = config.overlayAttrs.lbf-nix.lbfPlutusPurescript {
        name = "lbf-plutus-golden-api";
        src = ./.;
        files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
      };

    };
}
