self@{ inputs, ... }:
{
  perSystem = { pkgs, system, config, ... }:
    let
      pbnix-lib = inputs.protobufs-nix.lib.${system};
    in
    rec {

      devShells.dev-api = pkgs.mkShell {
        name = "protos-env";
        buildInputs = [
          pkgs.protobuf
          pkgs.haskellPackages.proto-lens-protoc
          pkgs.protoc-gen-doc
        ]; # ++ builtins.attrValues commonTools;

        shellHookd = ''
          export LC_CTYPE=C.UTF-8;
          export LC_ALL=C.UTF-8;
          export LANG=C.UTF-8;
          ${config.pre-commit.installationScript}
        '';
      };

      packages = {
        lambda-buffers-lang-hs-pb = pbnix-lib.haskellProto {
          inherit pkgs;
          src = ./.;
          proto = "lang.proto";
          cabalPackageName = "lambda-buffers-lang-pb";
        };

        lambda-buffers-compiler-hs-pb = pbnix-lib.haskellProto {
          inherit pkgs;
          src = ./.;
          proto = "compiler.proto";
          cabalBuildDepends = [ packages.lambda-buffers-lang-hs-pb ];
          cabalPackageName = "lambda-buffers-compiler-pb";
        };

        lambda-buffers-codegen-hs-pb = pbnix-lib.haskellProto {
          inherit pkgs;
          src = ./.;
          proto = "codegen.proto";
          cabalBuildDepends = [ packages.lambda-buffers-lang-hs-pb ];
          cabalPackageName = "lambda-buffers-codegen-pb";
        };

        lambda-buffers-api-docs = pkgs.stdenv.mkDerivation {
          src = ./.;
          name = "lambdabuffers-api-docs";
          buildInputs = [
            pkgs.protobuf
          ];
          buildPhase = ''
            mkdir $out;
            protoc --plugin=${pkgs.protoc-gen-doc}/bin/protoc-gen-doc lang.proto compiler.proto codegen.proto --doc_out=$out --doc_opt=markdown,api.md;
          '';
        };
      };
    };
}
