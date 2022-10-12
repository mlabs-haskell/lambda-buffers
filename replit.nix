{ pkgs }: {
    deps = [
        pkgs.cowsay
        pkgs.dhall
        pkgs.dhall-lsp-server
        pkgs.dhall-json
    ];
}