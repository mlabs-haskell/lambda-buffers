{ inputs, lib, ... }:
{
  perSystem = { pkgs, system, inputs', config, ... }:

    let
      pursFlake = config.overlayAttrs.extras.purescriptFlake {
        src = ./.;
        projectName = "lbt-prelude";
        strictComp = true;
        packageJson = ./package.json;
        packageLock = ./package-lock.json;

        extraSources = [
          config.packages.lbf-prelude-golden-api-purescript
          config.packages.lbf-prelude-purescript
          config.packages."purescript:lbr-prelude:src"
        ];
        data = [
          {
            name = "lbt-prelude-golden-data";
            path = config.packages.lbt-prelude-golden-purescript;
          }
        ];

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

      devShells.dev-lbt-prelude-purescript = pursFlake.devShell;

      inherit (pursFlake) packages checks;

    };
}
