# Foundational .lbf packages
# TODO(bladyjoker): Make packages that actually try and compile.
{ inputs, ... }:
{
  perSystem = { pkgs, config, ... }: {

    packages = {
      lbf-prelude = pkgs.stdenv.mkDerivation {
        name = "lbf-prelude";
        src = ./lbf-prelude;
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

      lbf-prelude-haskell = config.overlayAttrs.lbf-nix.lbfHaskell {
        name = "lbf-prelude";
        src = ./lbf-prelude;
        files = [ "Prelude.lbf" ];
        classes = [ "Prelude.Eq" "Prelude.Json" ];
        configs = [ "${config.packages.codegen-configs}/haskell-prelude-base.json" ];
      };

      lbf-prelude-purescript = config.overlayAttrs.lbf-nix.lbfPurescript {
        name = "lbf-prelude";
        src = ./lbf-prelude;
        files = [ "Prelude.lbf" ];
        classes = [ "Prelude.Eq" "Prelude.Json" ];
        configs = [ "${config.packages.codegen-configs}/purescript-prelude-base.json" ];
      };

      lbf-prelude-plutarch = config.overlayAttrs.lbf-nix.lbfPlutarch' {
        name = "lbf-prelude-plutarch";
        src = ./lbf-prelude;
        files = [ "Prelude.lbf" ];
        classes = [ "Prelude.Eq" ];
        configs = [ "${config.packages.codegen-configs}/plutarch-prelude.json" ];
      };

      lbf-plutus = pkgs.stdenv.mkDerivation {
        name = "lbf-plutus";
        src = ./lbf-plutus;
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

      lbf-plutus-haskell = config.overlayAttrs.lbf-nix.lbfHaskell {
        name = "lbf-plutus";
        src = ./lbf-plutus;
        imports = [ ./lbf-prelude ];
        files = [ "Plutus/V1.lbf" "Plutus/V2.lbf" ];
        classes = [ "Prelude.Eq" "Prelude.Json" "Plutus.V1.PlutusData" ];
        dependencies =
          [
            "lbf-prelude"
            # TODO(jaredponn): Investigate why `lbr-plutus` is _not_
            # being automatically included as a dependency.
            "lbr-plutus"
          ];
        configs = [
          "${config.packages.codegen-configs}/haskell-prelude-base.json"
          "${config.packages.codegen-configs}/haskell-plutus-plutustx.json"
        ];
      };

      lbf-plutus-purescript = config.overlayAttrs.lbf-nix.lbfPurescript {
        name = "lbf-plutus";
        src = ./lbf-plutus;
        imports = [ ./lbf-prelude ];
        files = [ "Plutus/V1.lbf" "Plutus/V2.lbf" ];
        classes = [ "Prelude.Eq" "Prelude.Json" "Plutus.V1.PlutusData" ];
        dependencies = [ "lbf-prelude" ];
        configs = [
          "${config.packages.codegen-configs}/purescript-prelude-base.json"
          "${config.packages.codegen-configs}/purescript-plutus-ctl.json"
        ];
      };

      lbf-plutus-plutarch = config.overlayAttrs.lbf-nix.lbfPlutarch' {
        name = "lbf-plutus-plutarch";
        src = ./lbf-plutus;
        imports = [ ./lbf-prelude ];
        files = [ "Plutus/V1.lbf" "Plutus/V2.lbf" ];
        classes = [ "Prelude.Eq" "Plutus.V1.PlutusData" ];
        dependencies = [ "lbf-prelude-plutarch" ];
        configs = [
          "${config.packages.codegen-configs}/plutarch-prelude.json"
          "${config.packages.codegen-configs}/plutarch-plutus.json"
        ];
      };
    };

    # The following devShells allow one to conveniently play with some of the
    # above schemas
    devShells = {
      dev-prelude-haskell =
        # Note:
        # `lbf-prelude-haskell` (defined above
        # `packages.lbf-prelude-haskell`) essentially generates a cabal
        # project from the `./Prelude.lbf` schema; and the following uses
        # `haskell.nix` to convert the `.cabal` project into a dev shell.
        # This is a dev shell which provides
        #   - ghc with `lbf-prelude-haskell` package (and its dependencies)
        #   - the CLI application (`lbf-prelude-to-haskell`) to compile `.lbf`
        #   schemas
        let
          project = { lib, ... }: {
            src = config.packages.lbf-prelude-haskell;

            name = "dev-prelude-haskell";

            inherit (config.settings.haskell) index-state compiler-nix-name;

            extraHackage = [
              "${config.packages.lbr-prelude-haskell-src}"
              "${config.packages.lbf-prelude-haskell}"
            ];

            modules = [
              (_: {
                packages = {
                  allComponent.doHoogle = true;
                  allComponent.doHaddock = true;
                };
              })
            ];

            shell = {

              withHoogle = true;

              exactDeps = true;

              nativeBuildInputs = config.settings.shell.tools
                ++ [ config.packages.lbf-prelude-to-haskell ];

              # Note: the `additional` (contrast to `packages`) attribute
              # includes the dependencies + the package itself. See:
              # https://input-output-hk.github.io/haskell.nix/reference/library.html#shellfor
              # This *must* be the name of the autogenerated cabal package from
              # `lbf-prelude-haskell`
              additional = ps: [ ps.lbf-prelude ];

              tools = {
                cabal = { };
                haskell-language-server = { };
              };

              shellHook = lib.mkForce config.settings.shell.hook;
            };
          };
          hsNixFlake = (pkgs.haskell-nix.cabalProject' [
            inputs.mlabs-tooling.lib.mkHackageMod
            inputs.mlabs-tooling.lib.moduleMod
            project
          ]).flake { };
        in
        hsNixFlake.devShell;

      dev-plutustx =
        # Note:
        # Similarly to `dev-prelude-haskell`, `packages.lbf-plutus-haskell`
        # essentially generates a cabal project from the `*.lbf` schemas; and
        # the following uses `haskell.nix` to convert the `.cabal` project into
        # a dev shell.
        # This is a dev shell which provides
        #   - ghc with `lbf-plutus-haskell` package (and its dependencies)
        #   - the CLI application (`lbf-plutus-to-haskell`) to compile `.lbf`
        #   schemas.
        #
        # Note:
        # This is mostly duplicated code from `dev-prelude-haskell`
        let
          project = { lib, ... }: {
            src = config.packages.lbf-plutus-haskell;

            name = "dev-plutustx";

            inherit (config.settings.haskell) index-state compiler-nix-name;

            extraHackage = [
              "${config.packages.lbr-prelude-haskell-src}"
              "${config.packages.lbf-prelude-haskell}"
              "${config.packages.lbr-plutus-haskell-src}"
              "${config.packages.lbf-plutus-haskell}"
            ];

            modules = [
              (_: {
                packages = {
                  allComponent.doHoogle = true;
                  allComponent.doHaddock = true;
                };
              })
            ];

            shell = {

              withHoogle = true;

              exactDeps = true;

              nativeBuildInputs = config.settings.shell.tools
                ++ [
                # We include both the Prelude and Plutus
                # frontend. Perhaps, we should _only_ include the
                # Plutus frontend, but it doesn't hurt to include both.
                config.packages.lbf-prelude-to-haskell
                config.packages.lbf-plutus-to-haskell
              ];

              additional = ps: [ ps.lbf-plutus ];

              tools = {
                cabal = { };
                haskell-language-server = { };
              };

              shellHook = lib.mkForce config.settings.shell.hook;
            };
          };
          hsNixFlake = (pkgs.haskell-nix.cabalProject' [
            inputs.mlabs-tooling.lib.mkHackageMod
            inputs.mlabs-tooling.lib.moduleMod
            project
          ]).flake { };
        in
        hsNixFlake.devShell;

      dev-plutarch =
        let
          project = { lib, ... }: {
            src = config.packages.lbf-plutus-plutarch;

            name = "dev-plutarch";

            inherit (config.settings.haskell) index-state compiler-nix-name;

            extraHackage = [
              # Load Plutarch support (Prelude, Plutus)
              "${config.packages.lbf-prelude-plutarch}"
              "${config.packages.lbf-plutus-plutarch}"
              "${config.packages.lbr-plutarch-src}"
              # Load Haskell support (Prelude, Plutus)
              "${config.packages.lbf-prelude-haskell}"
              "${config.packages.lbf-plutus-haskell}"
              "${config.packages.lbr-prelude-haskell-src}"
              "${config.packages.lbr-plutus-haskell-src}"
              # Plutarch itself
              "${inputs.plutarch}"
              "${inputs.plutarch}/plutarch-extra"
            ];

            modules = [
              (_: {
                packages = {
                  #allComponent.doHoogle = true;
                  #allComponent.doHaddock = true;

                  # lbf-prelude.configureFlags = [ "-f-dev" ];
                };
              })
            ];

            shell = {

              withHoogle = true;

              exactDeps = true;

              nativeBuildInputs = config.settings.shell.tools ++ [
                config.packages.lbf-plutus-to-plutarch
                config.packages.lbf-prelude-to-haskell
                config.packages.lbf-plutus-to-haskell
              ];

              additional = ps: [
                ps.lbf-prelude-plutarch
                ps.lbf-plutus-plutarch
                ps.lbr-plutarch
                ps.plutus-tx
                ps.plutus-ledger-api
              ];

              tools = {
                cabal = { };
                haskell-language-server = { };
              };

              shellHook = lib.mkForce config.settings.shell.hook;
            };
          };
          hsNixFlake = (pkgs.haskell-nix.cabalProject' [
            inputs.mlabs-tooling.lib.mkHackageMod
            inputs.mlabs-tooling.lib.moduleMod
            project
          ]).flake { };
        in
        hsNixFlake.devShell;

    };
  };
}

