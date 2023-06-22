pursProjOpts:
let
  mkFlake = projectName: purs: {
    packages = {
      "purescript:${projectName}:lib" = purs.compiled;
      "purescript:${projectName}:node-modules" = purs.nodeModules;
      "purescript:${projectName}:bundle" = purs.bundlePursProject { main = "Test.Main"; entrypoint = "app/index.js"; };
      "purescript:${projectName}:docs" = purs.buildPursDocs { };
      "purescript:${projectName}:docs-search" = purs.buildSearchablePursDocs { };
    };

    checks = {
      "purescript:${projectName}:check" = purs.runPursTest { };
    };

    devShell = purs.devShell;
  };
in
mkFlake pursProjOpts.projectName (pursProjOpts.pkgs.purescriptProject pursProjOpts)
