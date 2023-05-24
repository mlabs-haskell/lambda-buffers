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
    cabalBuildDepends = [ lambda-buffers-lang-hs-pb ];
    cabalPackageName = "lambda-buffers-compiler-pb";
  };

  lambda-buffers-codegen-hs-pb = pbnix-lib.haskellProto {
    inherit pkgs;
    src = ./.;
    proto = "codegen.proto";
    cabalBuildDepends = [ lambda-buffers-lang-hs-pb ];
    cabalPackageName = "lambda-buffers-compiler-pb";
  };

}
