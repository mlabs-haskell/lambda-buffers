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
        projectName = "lbr-prelude";
        strictComp = true;
        packageJson = ./package.json;
        packageLock = ./package-lock.json;
        shell = {
          withRuntime = false;
          packageLockOnly = true;
          packages = [
            pkgs.nodejs-18_x
            pkgs.bashInteractive
            pkgs.fd
          ] ++ config.settings.shell.tools;
          shellHook = config.settings.shell.hook;
        };
      };
    in
    {

      devShells.dev-lbr-prelude-purescript = pursFlake.devShell;
      inherit (pursFlake) packages checks;

    };
}
