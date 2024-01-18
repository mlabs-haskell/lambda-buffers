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
          name = "lbf-plutus-plutarch-golden-api";
          src = ./.;
          files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
        };

        lbf-plutus-golden-api-rust = config.lbf-nix.lbfPlutusRust {
          name = "lbf-plutus-rust-golden-api";
          src = ./.;
          files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
        };

      };
    };
}

