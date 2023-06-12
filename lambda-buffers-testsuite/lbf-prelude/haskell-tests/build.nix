{ pkgs
, haskell-nix
, mlabs-tooling
, lbr-prelude
, lbf-prelude
, lbfHaskell
, lbfPreludeHs
, compiler-nix-name
, index-state
, commonTools
, shellHook
}:
let
  inherit pkgs;

  lbfFooHs = lbfHaskell {
    src = ./..;
    inherit pkgs;
    cabalPackageName = "lbf-prelude-test";
    lbfFiles = [ "Foo/Bar.lbf" ];
    importPaths = [ lbf-prelude ];
    deps = [ lbfPreludeHs ];
  };
  project = { lib, ... }: {
    src = ./.;

    name = "lbf-prelude-haskell-tests";

    inherit compiler-nix-name index-state;

    extraHackage = [
      "${lbr-prelude}"
      "${lbfPreludeHs}"
      "${lbfFooHs}"
    ];

    modules = [
      (_: {
        packages = {
          allComponent.doHoogle = true;
          allComponent.doHaddock = true;

          # Enable strict compilation
          haskell-tests.configureFlags = [ "-f-dev" ];
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

      shellHook = lib.mkForce ''
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
