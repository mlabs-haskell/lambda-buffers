{ pkgs, shellHook }:
pkgs.mkShell {
  name = "experimental-env";
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.cowsay
    pkgs.dhall
    pkgs.dhall-lsp-server
    pkgs.dhall-json

    (pkgs.haskellPackages.ghcWithPackages (pkgs: [
      pkgs.text
    ]))
    pkgs.haskell-language-server

    pkgs.protobuf
    pkgs.haskellPackages.proto-lens-protoc
    pkgs.swiPrologWithGui
  ];

  inherit shellHook;
}
