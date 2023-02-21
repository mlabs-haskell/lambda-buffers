{ pkgs
, haskell-nix
, mlabs-tooling
, compiler-nix-name
, index-state
, compilerHsPb
, commonTools
, shellHook
}:
let
  inherit pkgs;
  tests = 10000;
  project = {
    src = ./.;

    name = "lambda-buffers-compiler";

    inherit compiler-nix-name index-state;

    extraHackage = [
      "${compilerHsPb}"
    ];

    modules = [
      (_: {
        packages = {
          allComponent.doHoogle = true;
          allComponent.doHaddock = true;

          # Enable strict compilation
          lambda-buffers-compiler.configureFlags = [ "-f-dev" ];

          # Set the number of QuickCheck and Hedgehog tests
          lambda-buffers-compiler.components.tests.lambda-buffers-compiler-tests.configureFlags = [
            "--hedgehog-tests=${tests}"
            "--quickcheck-tests=${tests}"
          ];
        };
      })
    ];

    shell = {

      withHoogle = true;

      exactDeps = true;

      nativeBuildInputs = builtins.attrValues commonTools;

      tools = {
        cabal = { };
        haskell-language-server = { };
      };

      shellHook = ''
        export LC_CTYPE=C.UTF-8
        export LC_ALL=C.UTF-8
        export LANG=C.UTF-8
        ${shellHook}
      '';
    };
  };
in
{
  hsNixProj = haskell-nix.cabalProject' [
    mlabs-tooling.lib.mkHackageMod
    project
  ];
}
