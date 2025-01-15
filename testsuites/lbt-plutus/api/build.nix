_: {
  perSystem = { config, ... }:
    {
      packages = {
        lbf-plutus-golden-api-haskell = config.lbf-nix.lbfPlutusHaskell {
          name = "lbf-plutus-golden-api";
          src = ./.;
          files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
        };

        lbf-plutus-golden-api-purescript = config.lbf-nix.lbfPlutusPurescript {
          name = "lbf-plutus-golden-api";
          src = ./.;
          files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
        };

        lbf-plutus-golden-api-plutarch = config.lbf-nix.lbfPlutarch {
          name = "lbf-plutus-golden-api-plutarch";
          src = ./.;
          files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
        };

        lbf-plutus-golden-api-plutustx = config.lbf-nix.lbfPlutusTx {
          name = "lbf-plutus-golden-api-plutustx";
          src = ./.;
          files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
        };

        lbf-plutus-golden-api-rust = config.lbf-nix.lbfPlutusRust {
          name = "lbf-plutus-golden-api";
          src = ./.;
          files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
        };

        lbf-plutus-golden-api-typescript = config.lbf-nix.lbfPlutusTypescript {
          name = "lbf-plutus-golden-api";
          src = ./.;
          files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
        };

      };
    };
}
