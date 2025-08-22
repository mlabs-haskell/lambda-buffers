_: {
  perSystem =
    {
      pkgs,
      config,
      inputs',
      ...
    }:
    {
      devShells.dev-experimental = pkgs.mkShell {
        name = "experimental-env";
        buildInputs = [
          pkgs.dhall
          pkgs.dhall-json

          (pkgs.haskellPackages.ghcWithPackages (hsPkgs: [
            hsPkgs.text
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
