_: {
  perSystem = { pkgs, config, ... }:
    {

      devShells.dev-experimental = pkgs.mkShell {
        name = "experimental-env";
        buildInputs = [
          pkgs.dhall
          pkgs.dhall-lsp-server
          pkgs.dhall-json

          (pkgs.haskellPackages.ghcWithPackages (pkgs: [
            pkgs.text
            pkgs.unification-fd
            pkgs.HUnit
          ]))
          pkgs.haskell-language-server

          pkgs.protobuf
          pkgs.haskellPackages.proto-lens-protoc
          pkgs.swiPrologWithGui
        ] ++ config.settings.shell.tools;

        shellHook = config.settings.shell.hook;
      };

    };
}
