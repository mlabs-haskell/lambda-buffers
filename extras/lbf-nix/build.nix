_: {
  perSystem = { pkgs, config, inputs', ... }:
    let
      lbg-haskell = "${config.packages.lbg-haskell}/bin/lbg-haskell";
      lbg-typescript = "${config.packages.lbg-typescript}/bin/lbg-typescript";
      lbg-plutarch = "${config.packages.lbg-plutarch}/bin/lbg-plutarch";
      lbg-purescript = "${config.packages.lbg-purescript}/bin/lbg-purescript";
      lbg-rust = "${config.packages.lbg-rust}/bin/lbg-rust";

    in
    {

      lbf-nix = {
        lbfBuild = import ./lbf-build.nix pkgs config.packages.lbf;
        lbfHaskell = import ./lbf-haskell.nix pkgs config.packages.lbf lbg-haskell;
        lbfPreludeHaskell = import ./lbf-prelude-hs.nix pkgs config.packages.lbf lbg-haskell;
        lbfPlutusHaskell = import ./lbf-plutus-hs-plutustx.nix pkgs config.packages.lbf lbg-haskell;
        lbfPlutarch' = import ./lbf-plutarch.nix pkgs config.packages.lbf lbg-plutarch;
        lbfPlutarch = import ./lbf-plutus-plutarch.nix pkgs config.packages.lbf lbg-plutarch;
        lbfPurescript = import ./lbf-purescript.nix pkgs config.packages.lbf lbg-purescript;
        lbfPreludePurescript = import ./lbf-prelude-purescript.nix pkgs config.packages.lbf lbg-purescript;
        lbfPlutusPurescript = import ./lbf-plutus-purescript.nix pkgs config.packages.lbf lbg-purescript;
        lbfTypescript = opts: import ./lbf-typescript.nix pkgs config.packages.lbf lbg-typescript opts;

        lbfPreludeTypescript = opts:
          import ./lbf-prelude-typescript.nix
            pkgs
            config.packages.lbf
            lbg-typescript
            config
            inputs'
            opts;
        # lbfPreludeTypescriptSrc = opts:
        #   (import ./lbf-prelude-typescript.nix
        #     pkgs
        #     config.packages.lbf
        #     lbg-typescript
        #     config
        #     opts).src;
        lbfRust = import ./lbf-rust.nix pkgs config.packages.lbf lbg-rust;
        lbfPreludeRust = import ./lbf-prelude-rust.nix pkgs config.packages.lbf lbg-rust;
        lbfPlutusRust = import ./lbf-plutus-rust.nix pkgs config.packages.lbf lbg-rust;
      };

    };
}
