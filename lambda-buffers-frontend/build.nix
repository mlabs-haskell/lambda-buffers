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

    name = "lambda-buffers-frontend";

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
          allComponent.configureFlags = [ "-f-dev" ];
        };
      })
    ];

    shell = {

      withHoogle = true;

      exactDeps = true;

      nativeBuildInputs = builtins.attrValues commonTools;

      additional = ps: [
        ps.lambda-buffers-compiler-pb
      ];

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
