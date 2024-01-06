# Creates a haskell.nix module that prepares a Cabal environment for building with Plutus.
compiler-nix-name: cardano-haskell-packages:
{ lib, config, pkgs, ... }:
let
  pkgs' = pkgs;
  brokenLibsModule =
    let
      responseFile = builtins.toFile "response-file" ''
        --optghc=-XFlexibleContexts
        --optghc=-Wwarn
        --optghc=-fplugin-opt=PlutusTx.Plugin:defer-errors
      '';
      l = [
        "cardano-binary"
        "cardano-crypto-class"
        "cardano-crypto-praos"
        "cardano-prelude"
        "heapwords"
        "measures"
        "strict-containers"
        "cardano-ledger-byron"
        "cardano-slotting"
      ];
    in
    {
      _file = "lambda-buffers/extras/haskell.nix/plutus.nix:brokenLibsModule";
      packages = builtins.listToAttrs (builtins.map
        (name: {
          inherit name;
          value.components.library = {
            setupHaddockFlags = [ "--haddock-options=@${responseFile}" ];
            ghcOptions = [ "-XFlexibleContexts" "-Wwarn" "-fplugin-opt=PlutusTx.Plugin:defer-errors" ];
            extraSrcFiles = [ responseFile ];
          };
        })
        l);
    };
  module = { pkgs, ... }: {
    _file = "lambda-buffers/extras/haskell.nix/plutus.nix:module";
    # FIXME: contentAddressed = true;
    reinstallableLibGhc = false; # See https://github.com/input-output-hk/haskell.nix/issues/1939
    packages = {
      cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
      cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
    };
  };
in
{
  _file = "lambda-buffers/extras/haskell.nix/plutus.nix";
  config = {
    cabalProjectLocal = builtins.readFile ./cabal.project.local;
    inherit compiler-nix-name;
    modules = [ module brokenLibsModule ];
    inputMap."https://input-output-hk.github.io/cardano-haskell-packages" = "${cardano-haskell-packages}";
    shell = {
      withHoogle = lib.mkOverride 999 false; # FIXME set to true
      exactDeps = lib.mkOverride 999 true;
      tools.haskell-language-server = { };
      # We use the ones from Nixpkgs, since they are cached reliably.
      # Eventually we will probably want to build these with haskell.nix.
      nativeBuildInputs = [
        pkgs'.cabal-install
      ];
    };
  };
}
