{ inputs, ... }: {
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
        ]; # ++ builtins.attrValues commonTools;

        shellHook = ''
          export LC_CTYPE=C.UTF-8;
          export LC_ALL=C.UTF-8;
          export LANG=C.UTF-8;
          ${config.pre-commit.installationScript}
        '';
      };

    };
}
