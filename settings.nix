# Repo wide settings
{
  lib,
  flake-parts-lib,
  inputs,
  ...
}:
{

  options = {

    perSystem = flake-parts-lib.mkPerSystemOption (
      {
        system,
        config,
        pkgs,
        ...
      }:
      {
        options.settings = {

          shell = {

            tools = lib.mkOption {
              type = lib.types.listOf lib.types.package;
              description = "Tools to include in all devShells";
            };

            hook = lib.mkOption {
              type = lib.types.str;
              description = "Shell script to invoke in all devShells";
            };
          };

          rust.localCrates = lib.mkOption {
            type = lib.types.attrs;
            description = "lbf-nix `extraVersions` pointing generated Rust crates at this repo's runtimes (siblings in `.extras`) instead of crates.io";
          };

          haskell = {

            index-state = lib.mkOption {
              type = lib.types.str;
              description = "Hackage index state to use when making a haskell.nix
 build environment";
            };

            compiler-nix-name = lib.mkOption {
              type = lib.types.str;
              description = "GHC Haskell compiler to use when building haskell.nix projects";
            };

            plutarch-src = lib.mkOption {
              type = lib.types.package;
              description = "Plutarch source tree (patched for the repo's GHC) to use as a haskell.nix dependency";
            };

            hbls-src = lib.mkOption {
              type = lib.types.package;
              description = "HBLS source";
            };

            proto-lens-deps = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              description = "proto-lens source package dirs (GHC 9.12-compatible 0.7.1.7) to inject as haskell.nix extraHackage dependencies";
            };

            modules = lib.mkOption {
              type = lib.types.listOf lib.types.raw;
              description = "Extra haskell.nix modules applied to plain (non-plutus) haskell.nix projects";
            };

          };

        };

        config = {

          settings = {

            haskell = {
              index-state = "2025-09-27T21:56:19Z";
              # GHC 9.12 is the only compiler that builds both plutarch 1.14
              # (needs >=9.8 for TypeAbstractions) and plutus-tx-plugin 1.65
              # (buildable only on 9.6.x or 9.12.x) for van Rossem/PV11.
              # Pinned to 9.12.1 specifically: 9.12.2's RTS crashes
              # (SRT_1 object entered!) when the Plinth plugin compiles on-chain
              # PlutusTx in the lbt-plutus-plutustx testsuite.
              compiler-nix-name = "ghc9121";

              # Plutarch 1.14 is warning-clean only on its tested GHC (9.8); its
              # -Weverything -Werror trips new warnings on GHC 9.12. We can't inject
              # ghc-options via cabal.project (plutarch is an extraHackage dep) nor
              # via haskell.nix modules (flake-lang's haskellPlutusFlake overwrites
              # the `modules` arg), so strip -Werror from plutarch's cabal files at
              # the source level instead.
              plutarch-src = pkgs.runCommand "plutarch-src-noWerror" { } ''
                cp -r ${inputs.plutarch} $out
                chmod -R +w $out
                find $out -name '*.cabal' -exec sed -i 's/-Werror//g' {} +
              '';

              hbls-src =
                let
                  src = pkgs.fetchgit {
                    url = "https://github.com/mlabs-haskell/hbls.git";
                    rev = "ecef8c65a74e5388b9e3e1f8c47892040967cad9";
                    fetchSubmodules = true;
                    sha256 = "sha256-8h6n1QO1ixpqxStSBI3Z2Fcgo9wiuPdaeN5cKVSl2Ow=";
                  };
                  # Must match the blst release hbls vendored (its cbits are v0.3.17's src/).
                  blst = pkgs.fetchFromGitHub {
                    owner = "supranational";
                    repo = "blst";
                    rev = "v0.3.17";
                    hash = "sha256-ROBlbVCiXgZsP694YjjzHzoFiGY+gtfz9fP5exzxEDA=";
                  };
                in
                # hbls vendors blst v0.3.17's C sources but not its assembly, and its
                # .cabal never compiles either, so loading hbls (e.g. the PlutusTx plugin)
                # hits undefined blst_* symbols. blst has no portable C path on
                # x86_64/aarch64, so add v0.3.17's build/ (asm) next to the vendored C
                # and compile both, as blst's own build.sh does. Nothing from the system.
                # ponytail: drop once hbls vendors build/ and declares these sources upstream.
                pkgs.runCommand "hbls-src" { } ''
                  cp -rL ${src} $out
                  chmod -R u+w $out
                  cp -r ${blst}/build $out/cbits/build
                  sed -i 's|^  cc-options: -O2$|&\n  c-sources: cbits/server.c\n  asm-sources: cbits/build/assembly.S|' $out/hbls.cabal
                  grep -q 'asm-sources: cbits/build/assembly.S' $out/hbls.cabal
                '';

              # proto-lens only gained GHC 9.12 support in 0.7.1.7 (2026-04), newer
              # than this repo's hackage index-state. Inject it (with git submodules,
              # which carry proto-lens-runtime's bundled descriptor.proto) as an
              # extraHackage dependency for the tool projects. Kept out of the
              # hackage input override to stay Hercules-CI compatible.
              proto-lens-deps =
                let
                  src = pkgs.fetchgit {
                    url = "https://github.com/google/proto-lens.git";
                    rev = "1b3bcd51f4f9236d6826f7f0a1c4387d90af04ce";
                    fetchSubmodules = true;
                    sha256 = "0wp2fk8s806aakhjr68i8zwvsl5w07ji4iwh3ms3ml48lx9a3bma";
                  };
                  # proto-lens' data-files reach the bundled protobuf .proto files via a
                  # symlink (proto-lens-imports/google -> ../../google/protobuf/...) to a
                  # sibling submodule dir. Extracting a single package subdir as an
                  # extraHackage dep dangles that symlink, so copy with -L to inline the
                  # real files and make each subdir self-contained.
                  pkg = name: pkgs.runCommand name { } ''cp -rL ${src}/${name} $out'';
                in
                [
                  "${pkg "proto-lens"}"
                  "${pkg "proto-lens-runtime"}"
                ];

              # freer-simple 1.2.1.2 (latest) has no GHC 9.12-compatible release;
              # its MonadBase instance is missing a `Monad b` constraint. Patch it.
              # These are top-level cabalProject' modules, so package-level config
              # goes inside their nested `modules` option.
              modules = [
                {
                  modules = [
                    { packages.freer-simple.patches = [ ./extras/patches/freer-simple-ghc912.patch ]; }
                  ];
                }
              ];
            };

            rust.localCrates = {
              lbr-prelude.path = "../lbr-prelude-v0";
              plutus-ledger-api = {
                path = "../plutus-ledger-api-3";
                features = [ "lbf" ];
              };
            };

            shell = {

              tools = [

                pkgs.haskellPackages.fourmolu
                pkgs.haskellPackages.hlint
                pkgs.haskellPackages.apply-refact

                pkgs.nil
                inputs.pre-commit-hooks.outputs.packages.${system}.deadnix
                inputs.pre-commit-hooks.outputs.packages.${system}.nixfmt-rfc-style

                inputs.pre-commit-hooks.outputs.packages.${system}.shellcheck

                inputs.pre-commit-hooks.outputs.packages.${system}.markdownlint-cli
                inputs.pre-commit-hooks.outputs.packages.${system}.dhall

                inputs.pre-commit-hooks.outputs.packages.${system}.purty

                # Required by spago2nix (shipped in the purescript dev shells) to
                # regenerate spago-packages.nix
                pkgs.nix-prefetch-git
              ];

              hook = ''
                export LC_CTYPE=C.UTF-8;
                export LC_ALL=C.UTF-8;
                export LANG=C.UTF-8;
                ${config.pre-commit.installationScript}
              '';
            };
          };
        };

      }
    );

  };

}
