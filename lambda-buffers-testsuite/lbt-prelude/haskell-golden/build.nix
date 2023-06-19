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
    src = ./../api;
    inherit pkgs;
    cabalPackageName = "lbf-golden-api";
    lbfFiles = [ "Foo/Bar.lbf" "Days.lbf" ];
    importPaths = [ lbf-prelude ];
    deps = [ lbfPreludeHs ];
  };
  goldenData = import ../../../extras/haskell-data.nix {
    inherit pkgs;
    srcs = [ ../. ];
    cabalDataPatterns = [ "**/*.lbf" "**/*.json" ];
    cabalPackageName = "lbt-prelude-golden-data-hs";
  };
  project = { lib, ... }: {
    src = ./.;

    name = "lbf-prelude-haskell-golden";

    inherit compiler-nix-name index-state;

    extraHackage = [
      "${lbr-prelude}"
      "${lbfPreludeHs}"
      "${lbfFooHs}"
      "${goldenData}"
    ];

    modules = [
      (_: {
        packages = {
          allComponent.doHoogle = true;
          allComponent.doHaddock = true;

          # Enable strict compilation
          haskell-golden.configureFlags = [ "-f-dev" ];
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
