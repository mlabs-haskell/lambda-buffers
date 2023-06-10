{ pkgs
, haskell-nix
, mlabs-tooling
, lbr-json-prelude
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
      "${lbr-json-prelude}"
    ];

    modules = [
      (_: {
        packages = {
          allComponent.doHoogle = true;
          allComponent.doHaddock = true;

          # Enable strict compilation
          lambda-buffers-compiler.configureFlags = [ "-f-dev" ];
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