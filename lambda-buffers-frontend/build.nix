{ pkgs
, haskell-nix
, mlabs-tooling
, compiler-nix-name
, index-state
, lambda-buffers-lang-hs-pb
, lambda-buffers-compiler-hs-pb
, lambda-buffers-codegen-hs-pb
, lambda-buffers-compiler
, lbc
, lbg
, lbg-haskell
, lbg-purescript
, commonTools
, shellHook
}:
let
  inherit pkgs;
  project = {
    src = ./.;

    name = "lambda-buffers-frontend";

    inherit compiler-nix-name index-state;

    extraHackage = [
      "${lambda-buffers-lang-hs-pb}"
      "${lambda-buffers-compiler-hs-pb}"
      "${lambda-buffers-codegen-hs-pb}"
      "${lambda-buffers-compiler}"
    ];

    modules = [
      (_: {
        packages = {
          allComponent.doHoogle = true;
          allComponent.doHaddock = true;

          # Enable strict compilation
          lambda-buffers-frontend.configureFlags = [ "-f-dev" ];
        };
      })
    ];

    shell = {

      withHoogle = true;

      exactDeps = true;

      nativeBuildInputs = [ lbc lbg lbg-haskell lbg-purescript ] ++ builtins.attrValues commonTools;

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
