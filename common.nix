({ inputs, lib, ... }: {
  imports = [
    ./pre-commit.nix # config.pre-commit options
  ];
  debug = true;
  perSystem = { pkgs, config, system, ... }: {
    options.common.tools = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      description = "Common tools available in all devShells.";
      default = with config.pre-commit.settings.tools; [
        nixpkgs-fmt
        cabal-fmt
        shellcheck
        hlint
        typos
        markdownlint-cli
        dhall
      ] ++
      [
        pkgs.protolint
        pkgs.txtpbfmt
        pkgs.haskell.packages.ghc925.fourmolu
        pkgs.haskell.packages.ghc925.apply-refact
      ];

    };

    options.common.shellHook = lib.mkOption {
      type = lib.types.str;
      description = "Common shell hook.";
      default = config.pre-commit.installationScript;
    };

    options.common.index-state = lib.mkOption {
      type = lib.types.str;
      description = "Hackage index state to build with.";
      default = "2022-12-01T00:00:00Z";
    };

    options.common.compiler-nix-name = lib.mkOption {
      type = lib.types.str;
      description = "GHC version to build with.";
      default = "ghc925";
    };


    # config._module.args.pkgs = import inputs.nixpkgs {
    #   inherit system;
    #   inherit (inputs.haskell-nix) config;
    #   overlays = [
    #     inputs.haskell-nix.overlay
    #     (import "${inputs.iohk-nix}/overlays/crypto")
    #   ];
    # };

  };

})
