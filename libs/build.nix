# Foundational .lbf packages
# TODO(bladyjoker): Make packages that actually try and compile.
{ inputs, ... }:
{
  perSystem = { pkgs, config, system, ... }: {

    packages = {
      lbf-prelude = pkgs.stdenv.mkDerivation {
        name = "lbf-prelude";
        src = ./lbf-prelude;
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

      lbf-prelude-haskell = config.lbf-nix.lbfHaskell {
        name = "lbf-prelude";
        src = ./lbf-prelude;
        files = [ "Prelude.lbf" ];
        classes = [ "Prelude.Eq" "Prelude.Json" ];
        configs = [ "${config.packages.codegen-configs}/haskell-prelude-base.json" ];
      };

      lbf-prelude-purescript = config.lbf-nix.lbfPurescript {
        name = "lbf-prelude";
        src = ./lbf-prelude;
        files = [ "Prelude.lbf" ];
        classes = [ "Prelude.Eq" "Prelude.Json" ];
        configs = [ "${config.packages.codegen-configs}/purescript-prelude-base.json" ];
      };

      lbf-prelude-typescript = config.lbf-nix.lbfTypescript {
        name = "lbf-prelude";
        src = ./lbf-prelude;
        files = [ "Prelude.lbf" ];
        classes = [ "Prelude.Eq" "Prelude.Json" ];
        configs = [ "${config.packages.codegen-configs}/typescript-prelude-base.json" ];
        npmExtraDependencies =
          [
            config.packages.lbr-prelude-typescript-tgz
          ];
      };

      lbf-prelude-plutarch = config.lbf-nix.lbfPlutarch' {
        name = "lbf-prelude-plutarch";
        src = ./lbf-prelude;
        files = [ "Prelude.lbf" ];
        classes = [ "Prelude.Eq" ];
        configs = [ "${config.packages.codegen-configs}/plutarch-prelude.json" ];
      };

      lbf-prelude-rust = config.lbf-nix.lbfRust {
        name = "lbf-prelude";
        src = ./lbf-prelude;
        files = [ "Prelude.lbf" ];
        classes = [ "Prelude.Eq" "Prelude.Json" ];
        configs = [ "${config.packages.codegen-configs}/rust-prelude-base.json" ];
      };

      lbf-plutus = pkgs.stdenv.mkDerivation {
        name = "lbf-plutus";
        src = ./lbf-plutus;
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

      lbf-plutus-haskell = config.lbf-nix.lbfHaskell {
        name = "lbf-plutus";
        src = ./lbf-plutus;
        imports = [ ./lbf-prelude ];
        files = [ "Plutus/V1.lbf" "Plutus/V1/Todo.lbf" "Plutus/V2.lbf" "Plutus/V2/Todo.lbf" ];
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

      lbf-plutus-purescript = config.lbf-nix.lbfPurescript {
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

      lbf-plutus-plutarch = config.lbf-nix.lbfPlutarch' {
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

      lbf-plutus-typescript = config.lbf-nix.lbfTypescript {
        name = "lbf-plutus";
        src = ./lbf-plutus;
        files = [ "Plutus/V1.lbf" "Plutus/V2.lbf" ];
        imports = { lbf-prelude = ./lbf-prelude; };
        classes = [ "Prelude.Eq" "Prelude.Json" "Plutus.V1.PlutusData" ];
        configs = [
          "${config.packages.codegen-configs}/typescript-prelude-base.json"
          "${config.packages.codegen-configs}/typescript-plutus.json"
        ];
        npmExtraDependencies =
          [
            config.packages.lbf-prelude-typescript
            config.packages.lbr-plutus-typescript-tgz
          ];
      };

      lbf-plutus-rust = config.lbf-nix.lbfRust {
        name = "lbf-plutus";
        src = ./lbf-plutus;
        imports = { lbf-prelude = ./lbf-prelude; };
        files = [ "Plutus/V1.lbf" "Plutus/V1/Todo.lbf" "Plutus/V2.lbf" "Plutus/V2/Todo.lbf" ];
        classes = [ "Prelude.Eq" "Prelude.Json" "Plutus.V1.PlutusData" ];
        configs = [
          "${config.packages.codegen-configs}/rust-prelude-base.json"
          "${config.packages.codegen-configs}/rust-plutus-pla.json"
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
          hsFlake = inputs.flake-lang.lib.${system}.haskellFlake {
            src = config.packages.lbf-prelude-haskell;

            name = "dev-prelude-haskell";

            inherit (config.settings.haskell) index-state compiler-nix-name;

            dependencies = [
              "${config.packages.lbr-prelude-haskell-src}"
              "${config.packages.lbf-prelude-haskell}"
            ];
            # Note: Add `lbf-prelude` Haskell package in the devShell environment.
            # This *must* be the name of the autogenerated cabal package from
            # `lbf-prelude-haskell`
            devShellAdditionalPackages = ps: [ ps.lbf-prelude ];

            devShellTools = config.settings.shell.tools ++ [ config.packages.lbf-prelude-to-haskell ];
            devShellHook = config.settings.shell.hook;
          };
        in
        hsFlake.devShell;

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
          hsFlake = inputs.flake-lang.lib.${system}.haskellPlutusFlake {
            src = config.packages.lbf-plutus-haskell;

            name = "dev-plutustx";

            inherit (config.settings.haskell) index-state compiler-nix-name;

            dependencies = [
              "${config.packages.lbr-prelude-haskell-src}"
              "${config.packages.lbf-prelude-haskell}"
              "${config.packages.lbr-plutus-haskell-src}"
              "${config.packages.lbf-plutus-haskell}"
            ];
            # Note: Add `lbf-prelude` and `lbf-plutus` Haskell packages in the devShell environment.
            # This *must* be the name of the autogenerated cabal package from
            # `lbf-prelude-haskell` and `lbf-plutus-haskell`
            devShellAdditionalPackages = ps: [ ps.lbf-prelude ps.lbf-plutus ];

            devShellTools = config.settings.shell.tools ++ [
              # We include both the Prelude and Plutus
              # frontend. Perhaps, we should _only_ include the
              # Plutus frontend, but it doesn't hurt to include both.
              config.packages.lbf-prelude-to-haskell
              config.packages.lbf-plutus-to-haskell
            ];

            devShellHook = config.settings.shell.hook;
          };
        in
        hsFlake.devShell;

      dev-plutarch =
        let
          hsFlake = inputs.flake-lang.lib.${system}.haskellPlutusFlake {
            src = config.packages.lbf-plutus-plutarch;

            name = "dev-plutarch";

            inherit (config.settings.haskell) index-state compiler-nix-name;

            dependencies = [
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
            devShellAdditionalPackages = ps: [
              ps.lbf-prelude-plutarch
              ps.lbf-plutus-plutarch
              ps.lbr-plutarch
              ps.plutus-tx
              ps.plutus-ledger-api
            ];

            devShellTools = config.settings.shell.tools ++ [
              config.packages.lbf-plutus-to-plutarch
              config.packages.lbf-prelude-to-haskell
              config.packages.lbf-plutus-to-haskell
            ];

            devShellHook = config.settings.shell.hook;
          };
        in
        hsFlake.devShell;
    };
  };
}

