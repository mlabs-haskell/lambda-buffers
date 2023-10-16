{ inputs, ... }:
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
        ] ++ config.settings.shell.tools;

        shellHook = config.settings.shell.hook;
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
