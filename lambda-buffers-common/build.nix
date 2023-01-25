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
  project = {
    src = ./.;

    name = "lambda-buffers-common";

    inherit compiler-nix-name index-state;

    extraHackage = [
      (builtins.toString compilerHsPb)
    ];

    modules = [
      (_: {
        packages = {
          allComponent.doHoogle = true;
          allComponent.doHaddock = true;

          # Enable strict compilation
          lambda-buffers-common.configureFlags = [ "-f-dev" ];
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
