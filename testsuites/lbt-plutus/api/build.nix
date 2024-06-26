_: {
  perSystem = { config, ... }:
    let
      mainPkgArgs = lang: ({
        name = "lbf-plutus-golden-api-${lang}";
        src = ./.;
        files = [ "Foo.lbf" "Foo/Bar.lbf" "DayTypes.lbf" ];
      } // (if (lang == "typescript")
      then {
        npmExtraDependencies = [ config.packages.lbf-plutus-golden-api-days-typescript ];
      }
      else {
        dependencies = [ "lbf-plutus-golden-api-days-${lang}" ];
      }));

      daysPkgArgs = lang: {
        name = "lbf-plutus-golden-api-days-${lang}";
        src = ./.;
        files = [ "Days.lbf" ];
      };
    in
    {
      packages = {
        lbf-plutus-golden-api-haskell = config.lbf-nix.lbfPlutusHaskell (mainPkgArgs "haskell");
        lbf-plutus-golden-api-days-haskell = config.lbf-nix.lbfPlutusHaskell (daysPkgArgs "haskell");

        lbf-plutus-golden-api-purescript = config.lbf-nix.lbfPlutusPurescript (mainPkgArgs "purescript");
        lbf-plutus-golden-api-days-purescript = config.lbf-nix.lbfPlutusPurescript (daysPkgArgs "purescript");

        lbf-plutus-golden-api-plutarch = config.lbf-nix.lbfPlutarch (mainPkgArgs "plutarch");
        lbf-plutus-golden-api-days-plutarch = config.lbf-nix.lbfPlutarch (daysPkgArgs "plutarch");

        lbf-plutus-golden-api-plutustx = config.lbf-nix.lbfPlutusTx (mainPkgArgs "plutustx");
        lbf-plutus-golden-api-days-plutustx = config.lbf-nix.lbfPlutusTx (daysPkgArgs "plutustx");

        lbf-plutus-golden-api-rust = config.lbf-nix.lbfPlutusRust (mainPkgArgs "rust");
        lbf-plutus-golden-api-days-rust = config.lbf-nix.lbfPlutusRust (daysPkgArgs "rust");

        lbf-plutus-golden-api-typescript = config.lbf-nix.lbfPlutusTypescript (mainPkgArgs "typescript");
        lbf-plutus-golden-api-days-typescript = config.lbf-nix.lbfPlutusTypescript (daysPkgArgs "typescript");

      };
    };
}

