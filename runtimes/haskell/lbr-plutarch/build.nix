{ inputs, ... }:
{
  perSystem = { pkgs, config, ... }:
    let
      project = { lib, ... }: {
        src = ./.;

        name = "lbr-plutarch";

        inherit (config.settings.haskell) index-state compiler-nix-name;
        extraHackage = [ "${inputs.plutarch}" "${inputs.plutarch}/plutarch-extra" ];
        modules = [
          (_: {
            packages = {
              allComponent.doHoogle = true;
              allComponent.doHaddock = true;

              # Enable strict compilation
              lbr-plutarch.configureFlags = [ "-f-dev" ];
            };
          })
        ];

        shell = {

          withHoogle = true;

          exactDeps = true;

          nativeBuildInputs = config.settings.shell.tools;

          tools = {
            cabal = { };
            haskell-language-server = { };
          };

          shellHook = lib.mkForce config.settings.shell.hook;
        };
      };
      hsNixFlake = (pkgs.haskell-nix.cabalProject' [
        inputs.mlabs-tooling.lib.mkHackageMod
        inputs.mlabs-tooling.lib.moduleMod
        project
      ]).flake { };

    in

    {
      devShells.dev-lbr-plutarch = hsNixFlake.devShell;

      packages = { };

    };
}
