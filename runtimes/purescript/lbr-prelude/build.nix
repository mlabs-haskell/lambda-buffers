{ pkgs, commonTools, shellHook }:
let
  projectName = "lbr-prelude";
  mkFlake = projectName: purs: {
    packages = {
      "purescript:${projectName}:lib" = purs.compiled;
      "purescript:${projectName}:node-modules" = purs.nodeModules;
      "purescript:${projectName}:bundle" = purs.bundlePursProject { };
      "purescript:${projectName}:docs" = purs.buildPursDocs { };
      "purescript:${projectName}:docs-search" = purs.buildSearchablePursDocs { };
    };

    checks = {
      "purescript:${projectName}:check" = purs.runPursTest { };
    };

    devShell = purs.devShell;
  };
  pursProj = pkgs.purescriptProject {
    inherit pkgs;
    src = ./.;
    inherit projectName;
    strictComp = true;
    packageJson = ./package.json;
    packageLock = ./package-lock.json;
    shell = {
      withRuntime = false;
      packageLockOnly = true;
      packages = builtins.attrValues commonTools ++ [
        pkgs.bashInteractive
        pkgs.fd
      ];
      shellHook = ''
        export LC_CTYPE=C.UTF-8
        export LC_ALL=C.UTF-8
        export LANG=C.UTF-8
        ${shellHook}
      '';
    };
  };

in
mkFlake projectName pursProj
