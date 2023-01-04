{ pkgs, pbnix-lib, commonTools, shellHook }:
{
  devShell = pkgs.mkShell {
    name = "protos-env";
    buildInputs = [
      pkgs.protobuf
      pkgs.haskellPackages.proto-lens-protoc
    ] ++ builtins.attrValues commonTools;

    inherit shellHook;
  };

  compilerHsPb = pbnix-lib.haskellProto {
    inherit pkgs;
    src = ./.;
    proto = "compiler.proto";
    cabalPackageName = "lambda-buffers-compiler-pb";
  };

}
