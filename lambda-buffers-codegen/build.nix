{ pkgs
, haskell-nix
, mlabs-tooling
, compiler-nix-name
, index-state
, compilerHsPb
, lambda-buffers-compiler
, commonTools
, shellHook
}:
let
  inherit pkgs;
  project = {
    src = ./.;

    name = "lambda-buffers-codegen";

    inherit compiler-nix-name index-state;

    extraHackage = [
      "${compilerHsPb}"
      "${lambda-buffers-compiler}"
    ];

    modules = [
      (_: {
        packages = {
          allComponent.doHoogle = true;
          allComponent.doHaddock = true;

          # Enable strict compilation
          lambda-buffers-codegen.configureFlags = [ "-f-dev" ];
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
