# Creates a haskell.nix module that prepares a Cabal environment for building with Plutus.
compiler-nix-name: cardano-haskell-packages:
{ lib, config, pkgs, ... }:
let
  pkgs' = pkgs;
  # https://github.com/input-output-hk/haskell.nix/issues/1177
  nonReinstallablePkgs = [
    "array"
    "base"
    "binary"
    "bytestring"
    "Cabal"
    "containers"
    "deepseq"
    "directory"
    "exceptions"
    "filepath"
    "ghc"
    "ghc-bignum"
    "ghc-boot"
    "ghc-boot"
    "ghc-boot-th"
    "ghc-compact"
    "ghc-heap"
    # "ghci"
    # "haskeline"
    "ghcjs-prim"
    "ghcjs-th"
    "ghc-prim"
    "ghc-prim"
    "hpc"
    "integer-gmp"
    "integer-simple"
    "mtl"
    "parsec"
    "pretty"
    "process"
    "rts"
    "stm"
    "template-haskell"
    "terminfo"
    "text"
    "time"
    "transformers"
    "unix"
    "Win32"
    "xhtml"
  ];
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
          value.components.library.setupHaddockFlags = [ "--haddock-options=@${responseFile}" ];
          value.components.library.ghcOptions = [ "-XFlexibleContexts" "-Wwarn" "-fplugin-opt=PlutusTx.Plugin:defer-errors" ];
          value.components.library.extraSrcFiles = [ responseFile ];
        })
        l);
    };
  module = { pkgs, ... }: {
    _file = "lambda-buffers/extras/haskell.nix/plutus.nix:module";
    # FIXME: contentAddressed = true;
    inherit nonReinstallablePkgs; # Needed for a lot of different things
    packages = {
      cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
      cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
    };
  };
in
{
  _file = "lambda-buffers/extras/haskell.nix/plutus.nix";
  config = {
    cabalProjectLocal = ''
      repository cardano-haskell-packages
        url: https://input-output-hk.github.io/cardano-haskell-packages
        secure: True
        root-keys:
        key-threshold: 0

      allow-newer:
        *:base,
        *:containers,
        *:directory,
        *:time,
        *:bytestring,
        *:aeson,
        *:protolude,
        *:template-haskell,
        *:ghc-prim,
        *:ghc,
        *:cryptonite,
        *:formatting,
        monoidal-containers:aeson,
        size-based:template-haskell,
        snap-server:attoparsec,
      --  tasty-hedgehog:hedgehog,
        *:hashable,
        *:text

      constraints:
        text >= 2
        , aeson >= 2
        , dependent-sum >= 0.7
        , protolude >= 0.3.2
        , nothunks >= 0.1.3

      package nothunks
        flags: +vector +bytestring +text
    '';
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
