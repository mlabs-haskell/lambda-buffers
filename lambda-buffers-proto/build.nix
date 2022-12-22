{ pkgs, pbnix-lib, shellHook }:
{
  protosDevShell = pkgs.mkShell {
    name = "protos-env";
    buildInputs = [
      pkgs.protobuf
      pkgs.protolint
      pkgs.txtpbfmt
      pkgs.haskellPackages.proto-lens-protoc
    ];

    inherit shellHook;
  };

  compilerHsPb = pbnix-lib.haskellProto {
    inherit pkgs;
    src = ./.;
    proto = "compiler.proto";
    cabalPackageName = "lambda-buffers-compiler-pb";
  };

}
