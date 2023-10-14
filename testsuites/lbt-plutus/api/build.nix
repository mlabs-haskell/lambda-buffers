{ inputs, lib, ... }: {
  perSystem = { pkgs, system, inputs', config, ... }:
    {
      packages.lbf-plutus-golden-api-haskell = config.overlayAttrs.lbf-nix.lbfPlutusHaskell {
        name = "lbf-plutus-golden-api";
        src = ./.;
        files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
      };

    };
}

