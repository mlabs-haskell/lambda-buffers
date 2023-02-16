{ pkgs
, haskell-nix
, mlabs-tooling
, compiler-nix-name
, index-state
, compilerHsPb
, lambda-buffers-compiler
, lambda-buffers-compiler-cli
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
      "${compilerHsPb}"
      "${lambda-buffers-compiler}"
    ];

    modules = [
      (_: {
        packages = {
          allComponent.doHoogle = true;
          allComponent.doHaddock = true;

          # Enable strict compilation
          lambda-buffers-frontend.configureFlags = [ "-f-dev" ];
          lambda-buffers-frontend.package.extraSrcFiles = [ "resources/**/*.lbf" ]; # TODO(bladyjoker): I would like to get rid of this as haskell-nix should pick it up from the Cabal file
        };
      })
    ];

    shell = {

      withHoogle = true;

      exactDeps = true;

      nativeBuildInputs = [ lambda-buffers-compiler-cli ] ++ builtins.attrValues commonTools;

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
