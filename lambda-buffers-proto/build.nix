{ pkgs, shellHook }:
pkgs.mkShell {
  name = "protos-env";
  buildInputs = [
    pkgs.protobuf
    pkgs.protolint
    pkgs.txtpbfmt
    pkgs.haskellPackages.proto-lens-protoc
  ];

  inherit shellHook;
}
