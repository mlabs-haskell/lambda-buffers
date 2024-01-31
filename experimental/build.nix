_: {
  perSystem = { pkgs, config, inputs', ... }:
    {

      devShells.dev-experimental = pkgs.mkShell {
        name = "experimental-env";
        buildInputs = [
          pkgs.dhall
          # TODO(bladyjoker): error: Package ‘dhall-lsp-server-1.1.3’ in /nix/store/p7iz0r8gs6ppkhj83zjmwyd21k8b7v3y-source/pkgs/development/haskell-modules/hackage-packages.nix:84957 is marked as broken, refusing to evaluate.
          # pkgs.dhall-lsp-server
          pkgs.dhall-json

          (pkgs.haskellPackages.ghcWithPackages (hsPkgs: [
            hsPkgs.text
            hsPkgs.unification-fd
            hsPkgs.HUnit
          ]))

          pkgs.protobuf
          pkgs.haskellPackages.haskell-language-server
        ]
        ++ (pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.swiPrologWithGui ])
        ++ config.settings.shell.tools;

        shellHook = config.settings.shell.hook;
        inputsFrom = [ inputs'.proto-nix.devShells.dev-proto-nix ];
      };

    };
}
