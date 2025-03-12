{ lib, inputs, ... }:
{
  perSystem =
    {
      pkgs,
      config,
      system,
      ...
    }:
    let
      lbg-haskell = "${config.packages.lbg-haskell}/bin/lbg-haskell";
      lbg-typescript = "${config.packages.lbg-typescript}/bin/lbg-typescript";
      lbg-plutarch = "${config.packages.lbg-plutarch}/bin/lbg-plutarch";
      lbg-plutustx = "${config.packages.lbg-plutustx}/bin/lbg-plutustx";
      lbg-purescript = "${config.packages.lbg-purescript}/bin/lbg-purescript";
      lbg-rust = "${config.packages.lbg-rust}/bin/lbg-rust";

    in
    {

      lbf-nix = {
        lbfBuild = import ./lbf-build.nix pkgs config.packages.lbf;
        lbfHaskell = import ./lbf-haskell.nix pkgs config.packages.lbf lbg-haskell;
        lbfPreludeHaskell = import ./lbf-prelude-hs.nix pkgs config.packages.lbf lbg-haskell;
        lbfPlutusHaskell = import ./lbf-plutus-hs.nix pkgs config.packages.lbf lbg-haskell;
        lbfPlutarchBase = import ./lbf-plutarch-base.nix pkgs config.packages.lbf lbg-plutarch;
        lbfPlutarch = import ./lbf-plutarch.nix pkgs config.packages.lbf lbg-plutarch;
        lbfPlutusTxBase = import ./lbf-plutustx-base.nix pkgs config.packages.lbf lbg-plutustx;
        lbfPlutusTx = import ./lbf-plutustx.nix pkgs config.packages.lbf lbg-plutustx;
        lbfPurescript = import ./lbf-purescript.nix pkgs config.packages.lbf lbg-purescript;
        lbfPreludePurescript = import ./lbf-prelude-purescript.nix pkgs config.packages.lbf lbg-purescript;
        lbfPlutusPurescript = import ./lbf-plutus-purescript.nix pkgs config.packages.lbf lbg-purescript;
        lbfTypescript =
          opts:
          import ./lbf-typescript.nix {
            inherit pkgs lbg-typescript;
            inherit (config.packages) lbf-list-modules-typescript lbf;
            inherit (inputs.flake-lang.lib.${system}) typescriptFlake;
          } opts;
        lbfPreludeTypescript =
          opts:
          import ./lbf-prelude-typescript.nix {
            inherit pkgs lbg-typescript config;
            inherit (config.packages) lbf-list-modules-typescript lbf;
            inherit (inputs.flake-lang.lib.${system}) typescriptFlake;
          } opts;
        lbfPlutusTypescript =
          opts:
          import ./lbf-plutus-typescript.nix {
            inherit pkgs lbg-typescript config;
            inherit (config.packages) lbf-list-modules-typescript lbf;
            inherit (inputs.flake-lang.lib.${system}) typescriptFlake;
          } opts;
        lbfRust = import ./lbf-rust.nix pkgs config.packages.lbf lbg-rust;
        lbfPreludeRust = import ./lbf-prelude-rust.nix pkgs config.packages.lbf lbg-rust;
        lbfPlutusRust = import ./lbf-plutus-rust.nix pkgs config.packages.lbf lbg-rust;
      };

      packages = {
        lbf-list-modules-typescript = pkgs.stdenv.mkDerivation {
          name = "lbf-list-modules-typescript";
          buildInputs = [ pkgs.nodejs ];
          dontUnpack = true;
          src = ./lbf-list-modules-typescript;
          installPhase = ''
            mkdir -p "$out/bin"
            cp "$src" "$out/bin/lbf-list-modules-typescript"
          '';
        };
      };

    };
}
