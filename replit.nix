{ pkgs }: {
    deps = [
        pkgs.haskellPackages.ghc
        pkgs.cowsay
        pkgs.dhall
        pkgs.dhall-lsp-server
        pkgs.dhall-json

        (pkgs.haskellPackages.ghcWithPackages (pkgs: [
          pkgs.text
        ]))
        pkgs.haskell-language-server      
    ];
  
}