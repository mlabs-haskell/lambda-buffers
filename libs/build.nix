# Foundational .lbf packages
# TODO(bladyjoker): Make packages that actually try and compile.
{
  perSystem =
    { pkgs, config, ... }:
    {

      packages = {
        lbf-prelude = pkgs.stdenv.mkDerivation {
          pname = "lbf-prelude";
          version = "1.0.0";
          src = ./lbf-prelude;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

        lbf-prelude-haskell = config.lbf-nix.lbfHaskell {
          name = "lbf-prelude";
          src = ./lbf-prelude;
          files = [ "Prelude.lbf" ];
          classes = [
            "Prelude.Eq"
            "Prelude.Json"
          ];
          configs = [ "${config.packages.codegen-configs}/haskell-prelude-base.json" ];
        };

        lbf-prelude-purescript = config.lbf-nix.lbfPurescript {
          name = "lbf-prelude";
          src = ./lbf-prelude;
          files = [ "Prelude.lbf" ];
          classes = [
            "Prelude.Eq"
            "Prelude.Json"
          ];
          configs = [ "${config.packages.codegen-configs}/purescript-prelude-base.json" ];
        };

        lbf-prelude-typescript = config.lbf-nix.lbfTypescript {
          name = "lbf-prelude";
          src = ./lbf-prelude;
          files = [ "Prelude.lbf" ];
          classes = [
            "Prelude.Eq"
            "Prelude.Json"
          ];
          configs = [ "${config.packages.codegen-configs}/typescript-prelude-base.json" ];
          npmExtraDependencies = [
            config.packages.lbr-prelude-typescript-lib
          ];
        };

        lbf-prelude-plutarch = config.lbf-nix.lbfPlutarchBase {
          name = "lbf-prelude-plutarch";
          src = ./lbf-prelude;
          files = [ "Prelude.lbf" ];
          classes = [ "Prelude.Eq" ];
          configs = [ "${config.packages.codegen-configs}/plutarch-prelude.json" ];
        };

        lbf-prelude-plutustx = config.lbf-nix.lbfPlutusTxBase {
          name = "lbf-prelude-plutustx";
          src = ./lbf-prelude;
          files = [ "Prelude.lbf" ];
          classes = [ "Prelude.Eq" ];
          configs = [ "${config.packages.codegen-configs}/plutustx-prelude.json" ];
        };

        lbf-prelude-rust = config.lbf-nix.lbfRust {
          name = "lbf-prelude";
          src = ./lbf-prelude;
          files = [ "Prelude.lbf" ];
          classes = [
            "Prelude.Eq"
            "Prelude.Json"
          ];
          configs = [ "${config.packages.codegen-configs}/rust-prelude-base.json" ];
        };

        lbf-plutus = pkgs.stdenv.mkDerivation {
          pname = "lbf-plutus";
          version = "1.0.0";
          src = ./lbf-plutus;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

        lbf-plutus-haskell = config.lbf-nix.lbfHaskell {
          name = "lbf-plutus";
          src = ./lbf-plutus;
          imports = [ ./lbf-prelude ];
          files = [
            "Plutus/V1.lbf"
            "Plutus/V2.lbf"
            "Plutus/V3.lbf"
          ];
          classes = [
            "Prelude.Eq"
            "Prelude.Json"
            "Plutus.V1.PlutusData"
          ];
          dependencies = [
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
          files = [
            "Plutus/V1.lbf"
            "Plutus/V2.lbf"
            "Plutus/V3.lbf"
          ];
          classes = [
            "Prelude.Eq"
            "Prelude.Json"
            "Plutus.V1.PlutusData"
          ];
          dependencies = [ "lbf-prelude" ];
          configs = [
            "${config.packages.codegen-configs}/purescript-prelude-base.json"
            "${config.packages.codegen-configs}/purescript-plutus-ctl.json"
          ];
        };

        lbf-plutus-plutarch = config.lbf-nix.lbfPlutarchBase {
          name = "lbf-plutus-plutarch";
          src = ./lbf-plutus;
          imports = [ ./lbf-prelude ];
          files = [
            "Plutus/V1.lbf"
            "Plutus/V2.lbf"
            "Plutus/V3.lbf"
          ];
          classes = [
            "Prelude.Eq"
            "Plutus.V1.PlutusData"
          ];
          dependencies = [ "lbf-prelude-plutarch" ];
          configs = [
            "${config.packages.codegen-configs}/plutarch-prelude.json"
            "${config.packages.codegen-configs}/plutarch-plutus.json"
          ];
        };

        lbf-plutus-plutustx = config.lbf-nix.lbfPlutusTxBase {
          name = "lbf-plutus-plutustx";
          src = ./lbf-plutus;
          imports = [ ./lbf-prelude ];
          files = [
            "Plutus/V1.lbf"
            "Plutus/V2.lbf"
            "Plutus/V3.lbf"
          ];
          classes = [
            "Prelude.Eq"
            "Plutus.V1.PlutusData"
          ];
          dependencies = [ "lbf-prelude-plutustx" ];
          configs = [
            "${config.packages.codegen-configs}/plutustx-prelude.json"
            "${config.packages.codegen-configs}/plutustx-plutus.json"
          ];
        };

        lbf-plutus-typescript = config.lbf-nix.lbfTypescript {
          name = "lbf-plutus";
          src = ./lbf-plutus;
          files = [
            "Plutus/V1.lbf"
            "Plutus/V2.lbf"
            "Plutus/V3.lbf"
          ];
          imports = {
            lbf-prelude = ./lbf-prelude;
          };
          classes = [
            "Prelude.Eq"
            "Prelude.Json"
            "Plutus.V1.PlutusData"
          ];
          configs = [
            "${config.packages.codegen-configs}/typescript-prelude-base.json"
            "${config.packages.codegen-configs}/typescript-plutus.json"
          ];
          npmExtraDependencies = [
            config.packages.lbf-prelude-typescript
            config.packages.lbr-plutus-typescript-lib
          ];
        };

        lbf-plutus-rust = config.lbf-nix.lbfRust {
          name = "lbf-plutus";
          src = ./lbf-plutus;
          imports = {
            lbf-prelude = ./lbf-prelude;
          };
          files = [
            "Plutus/V1.lbf"
            "Plutus/V2.lbf"
            "Plutus/V3.lbf"
          ];
          classes = [
            "Prelude.Eq"
            "Prelude.Json"
            "Plutus.V1.PlutusData"
          ];
          configs = [
            "${config.packages.codegen-configs}/rust-prelude-base.json"
            "${config.packages.codegen-configs}/rust-plutus-pla.json"
          ];
        };
      };
    };
}
