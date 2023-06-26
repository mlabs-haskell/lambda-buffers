{ pkgs
, haskell-nix
, mlabs-tooling
, lbr-prelude
, compiler-nix-name
, index-state
, commonTools
, shellHook
}:
let
  inherit pkgs;
  project = { lib, ... }: {
    src = ./.;

    name = "lbr-json-plutus";

    inherit compiler-nix-name index-state;

    extraHackage = [
      "${lbr-prelude}"
    ];

    modules = [
      (_: {
        packages = {
          allComponent.doHoogle = true;
          allComponent.doHaddock = true;

          # Enable strict compilation
          lbr-plutus.configureFlags = [ "-f-dev" ];
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
    mlabs-tooling.lib.moduleMod
    project
  ];
}
