{ pkgs
, haskell-nix
, mlabs-tooling
, lbf-prelude
, lbf-prelude-hs
, lbr-prelude-hs
, lbfHaskell
, compiler-nix-name
, index-state
, commonTools
, shellHook
}:
let
  inherit pkgs;

  goldenApiHs = lbfHaskell {
    inherit pkgs;
    name = "lbf-golden-api";
    src = ./../api;
    files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
    imports = [ ../../../libs/lbf-prelude ];
    dependencies = [ "lbf-prelude" "lbr-prelude" ];
  };

  goldenData = import ../../../extras/haskell-data.nix {
    inherit pkgs;
    srcs = [ ../. ];
    cabalDataPatterns = [ "**/*.lbf" "**/*.json" ];
    cabalPackageName = "lbt-prelude-golden-data-hs";
  };

  project = { lib, ... }: {
    src = ./.;

    name = "lbt-prelude-haskell";

    inherit compiler-nix-name index-state;

    extraHackage = [
      "${lbr-prelude-hs}"
      "${lbf-prelude-hs}"
      "${goldenApiHs}"
      "${goldenData}"
    ];

    modules = [
      (_: {
        packages = {
          allComponent.doHoogle = true;
          allComponent.doHaddock = true;

          # Enable strict compilation
          lbt-prelude-haskell.configureFlags = [ "-f-dev" ];
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
