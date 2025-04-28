{ inputs, ... }:
{
  perSystem =
    {
      pkgs,
      config,
      system,
      ...
    }:

    let
      pursFlake = inputs.flake-lang.lib.${system}.purescriptFlake {
        inherit pkgs;
        src = ./.;
        projectName = "lbr-plutus";
        strictComp = true;
        packageJson = ./package.json;
        packageLock = ./package-lock.json;
        shell = {
          withRuntime = false;
          packageLockOnly = true;
          packages = [
            pkgs.nodejs
            pkgs.bashInteractive
            pkgs.fd
          ] ++ config.settings.shell.tools;
          shellHook = config.settings.shell.hook;
        };
      };
    in
    {

      devShells.dev-lbr-plutus-purescript = pursFlake.devShell;

      inherit (pursFlake) packages checks;

    };
}
