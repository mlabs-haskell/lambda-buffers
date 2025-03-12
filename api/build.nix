{ inputs, ... }:
{
  perSystem =
    {
      pkgs,
      system,
      config,
      inputs',
      ...
    }:
    let
      proto-nix = inputs.proto-nix.lib.${system};
    in
    rec {

      devShells.dev-api = pkgs.mkShell {
        name = "protos-env";
        inputsFrom = [ inputs'.proto-nix.devShells.dev-proto-nix ];
        shellHook = config.settings.shell.hook;
      };

      packages = {
        lambda-buffers-lang-hs-pb = proto-nix.haskellProto {
          src = ./.;
          protos = [ "lang.proto" ];
          cabalPackageName = "lambda-buffers-lang-pb";
        };

        lambda-buffers-compiler-hs-pb = proto-nix.haskellProto {
          src = ./.;
          protos = [ "compiler.proto" ];
          cabalBuildDepends = [ packages.lambda-buffers-lang-hs-pb ];
          cabalPackageName = "lambda-buffers-compiler-pb";
        };

        lambda-buffers-codegen-hs-pb = proto-nix.haskellProto {
          src = ./.;
          protos = [ "codegen.proto" ];
          cabalBuildDepends = [ packages.lambda-buffers-lang-hs-pb ];
          cabalPackageName = "lambda-buffers-codegen-pb";
        };

        lambda-buffers-api-docs = proto-nix.docProto {
          src = ./.;
          protos = [
            "lang.proto"
            "compiler.proto"
            "codegen.proto"
          ];
        };
      };
    };
}
