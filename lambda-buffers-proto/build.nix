{ pkgs, shellHook }:
pkgs.mkShell {
  name = "protos-env";
  buildInputs = [
    pkgs.protobuf
    pkgs.haskellPackages.proto-lens-protoc
  ];

  inherit shellHook;
}
