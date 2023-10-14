{ inputs, lib, ... }: {
  perSystem = { pkgs, system, inputs', config, ... }:
    {
      packages.lbf-prelude-golden-api-haskell = config.overlayAttrs.lbf-nix.lbfPreludeHaskell {
        name = "lbf-prelude-golden-api";
        src = ./.;
        files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
      };

    };
}

