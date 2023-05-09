{ inputs, ... }:
{
  imports = [
    #  ../common.nix
  ];

  perSystem = { pkgs, system, config, ... }:
    {
      devShells.dev-protos = pkgs.mkShell {
        name = "protos-env";
        buildInputs = [
          pkgs.protobuf
          pkgs.haskellPackages.proto-lens-protoc
          pkgs.protoc-gen-doc
        ] ++ config.common.tools;

        inherit (config.common) shellHook;
      };

      packages = {
        lbHsPb = inputs.protobufs-nix.lib.${system}.haskellProto {
          inherit pkgs;
          src = ./.;
          proto = "lambdabuffers.proto";
          cabalPackageName = "lambda-buffers-pb";
        };

        compilerHsPb = inputs.protobufs-nix.lib.${system}.haskellProto {
          inherit pkgs;
          src = ./.;
          proto = "compiler.proto";
          cabalPackageName = "lambda-buffers-compiler-pb";
        };

        codegenHsPb = inputs.protobufs-nix.lib.${system}.haskellProto {
          inherit pkgs;
          src = ./.;
          proto = "codegen.proto";
          cabalPackageName = "lambda-buffers-codegen-pb";
        };
      };

    };
}
