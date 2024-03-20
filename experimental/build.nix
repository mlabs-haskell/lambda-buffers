_: {
  perSystem = { pkgs, config, inputs', ... }:
    let
      hpkgs = pkgs.haskellPackages.override {
        overrides = hself: hsuper: {
          # Can add/override packages here
          logict_0_8_0_0 =
            pkgs.haskell.lib.overrideCabal hsuper.logict {
              version = "0.8.0.0";
              sha256 = "sha256-/pJt8pW8Q995Qkc2DnoKDf3HeLzphviq26mP5SIo+1Y=";
            };
          unification-fd = # pkgs.haskell.lib.unmarkBroken hsuper.unification-fd;
            pkgs.haskell.lib.overrideCabal hsuper.unification-fd (_: {
              libraryHaskellDepends = [
                hsuper.base
                hsuper.containers
                hself.logict_0_8_0_0
                hsuper.mtl
              ];
              broken = false;
            });
        };
      };
    in
    {
      devShells.dev-experimental = pkgs.mkShell {
        name = "experimental-env";
        buildInputs = [
          pkgs.dhall
          # TODO(bladyjoker): error: Package ‘dhall-lsp-server-1.1.3’ in /nix/store/p7iz0r8gs6ppkhj83zjmwyd21k8b7v3y-source/pkgs/development/haskell-modules/hackage-packages.nix:84957 is marked as broken, refusing to evaluate.
          # pkgs.dhall-lsp-server
          pkgs.dhall-json

          (hpkgs.ghcWithPackages (hsPkgs: [
            hsPkgs.text
            hsPkgs.unification-fd
            hsPkgs.HUnit
          ]))

          pkgs.protobuf
          hpkgs.haskell-language-server
        ]
        ++ (pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.swiPrologWithGui ])
        ++ config.settings.shell.tools;

        shellHook = config.settings.shell.hook;
        inputsFrom = [ inputs'.proto-nix.devShells.dev-proto-nix ];
      };

    };
}
