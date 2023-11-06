{ inputs, ... }: {
  perSystem = { self', pkgs, system, ... }:
    let
      rustProject = { crateName, localDeps, dataDeps }:
        let
          rustWithTools = pkgs.rust-bin.stable.latest.default.override {
            extensions = [ "rustfmt" "rust-analyzer" "clippy" ];
          };
          craneLib = inputs.crane.lib.${system}.overrideToolchain rustWithTools;

          buildEnv = pkgs.stdenv.mkDerivation {
            src = ./.;
            name = "lbf-rust-workspace";
            unpackPhase = builtins.concatStringsSep "\n"
              ([
                "mkdir -p $out"
                "cp -r $src $out/${crateName}"
              ]
              ++ (map ({ name, path }: "cp -r ${path} $out/${name}") localDeps)
              ++ (map ({ name, path }: "cp -r ${path} $out/${name}") dataDeps)
              );
          };
          commonArgs = {
            src = buildEnv;
            pname = crateName;
            strictDeps = true;
            cargoLock = ./Cargo.lock;
            cargoToml = ./Cargo.toml;
            postUnpack = ''
              cd $sourceRoot/${crateName}
              sourceRoot="."
            '';
          };
          cargoArtifacts = craneLib.buildDepsOnly commonArgs;

        in
        {
          devShells."dev-${crateName}-rust" = craneLib.devShell {
            checks = self'.checks;
          };

          packages."${crateName}-rust" = craneLib.buildPackage commonArgs // {
            inherit cargoArtifacts;
            doCheck = false;
          };

          checks."${crateName}-rust-test" = craneLib.cargoNextest (commonArgs // {
            inherit cargoArtifacts;
          });

          checks."${crateName}-rust-clippy" = craneLib.cargoClippy (commonArgs // {
            inherit cargoArtifacts;
          });
        };
    in
    rustProject {
      crateName = "lbr-prelude-derive";
      localDeps = [
        {
          name = "lbr-prelude";
          path = ../lbr-prelude;
        }
      ];
      dataDeps = [ ];
    };

}
