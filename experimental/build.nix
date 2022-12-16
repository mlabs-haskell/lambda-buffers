{ pkgs, preCommitTools, shellHook }:
pkgs.mkShell {
  name = "experimental-env";
  buildInputs = [
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

    preCommitTools.fourmolu
    preCommitTools.typos
    preCommitTools.cabal-fmt
    preCommitTools.hlint
  ];

  inherit shellHook;
}
