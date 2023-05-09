{ inputs, ... }:
let
  project = pkgs: config: {
    src = ./.;

    name = "lambda-buffers-compiler";

    inherit (config.common) compiler-nix-name index-state;

    extraHackage = [
      # "${config.packages.compilerHsPb}"
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

      nativeBuildInputs = [ pkgs.swiPrologWithGui ] ++ config.common.tools;

      tools = {
        cabal = { };
        haskell-language-server = { };
      };

      inherit (config.common) shellHook;
    };
  };
in
{
  imports = [
    #../common.nix
  ];

  perSystem = { pkgs, system, config, ... }:
    let
      pkgs' = import inputs.nixpkgs {
        inherit system;
        inherit (inputs.haskell-nix) config;
        overlays = [
          inputs.haskell-nix.overlay
          (import "${inputs.iohk-nix}/overlays/crypto")
        ];
      };

    in
    {
      packages = ((pkgs'.haskell-nix.cabalProject' [
        inputs.mlabs-tooling.lib.mkHackageMod
        (project pkgs config)
        {
          src = ./.;

          name = "lambda-buffers-compiler";

          inherit (config.common) compiler-nix-name index-state;

          extraHackage = [
            # "${config.packages.compilerHsPb}"
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

            nativeBuildInputs = [ pkgs.swiPrologWithGui ] ++ config.common.tools;

            tools = {
              cabal = { };
              haskell-language-server = { };
            };

            inherit (config.common) shellHook;
          };
        }
      ]).flake { }).packages;
    };
}
