_:
{
  perSystem = { pkgs, config, ... }:

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
          packages = [
            pkgs.nodejs_16
            pkgs.bashInteractive
            pkgs.fd
          ] ++ config.settings.shell.tools;
          shellHook = config.settings.shell.hook;
        };

      };
    in
    {

      devShells.dev-lbt-prelude-purescript = pursFlake.devShell;

      inherit (pursFlake) packages checks;

    };
}
