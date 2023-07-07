{ pkgs, pbnix-lib, commonTools, shellHook }:
rec {
  devShell = pkgs.mkShell {
    name = "protos-env";
    buildInputs = [
      pkgs.protobuf
      pkgs.haskellPackages.proto-lens-protoc
      pkgs.protoc-gen-doc
    ] ++ builtins.attrValues commonTools;

    inherit shellHook;
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
}
