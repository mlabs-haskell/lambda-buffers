{ inputs, lib, ... }:
{
  perSystem = { pkgs, system, inputs', config, ... }:

    let
      pursFlake = config.overlayAttrs.extras.purescriptFlake {
        inherit pkgs;
        src = ./.;
        projectName = "lbr-prelude";
        strictComp = true;
        packageJson = ./package.json;
        packageLock = ./package-lock.json;
        shell = {
          withRuntime = false;
          packageLockOnly = true;
          packages = #builtins.attrValues commonTools ++ [
            [
              pkgs.nodejs_16
              pkgs.bashInteractive
              pkgs.fd
            ];
          shellHook = ''
            export LC_CTYPE=C.UTF-8;
            export LC_ALL=C.UTF-8;
            export LANG=C.UTF-8;
            ${config.pre-commit.installationScript}
          '';
        };
      };
    in
    {

      devShells.dev-lbr-prelude-purescript = pursFlake.devShell;
      inherit (pursFlake) packages checks;

    };
}
