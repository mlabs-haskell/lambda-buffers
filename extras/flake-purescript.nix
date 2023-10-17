pkgs: pursProjOpts:
let
  mkFlake = projectName: purs: {
    packages = {
      "purescript:${projectName}:src" = pkgs.stdenv.mkDerivation {
        name = projectName;
        inherit (pursProjOpts) src;
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };
      "purescript:${projectName}:lib" = purs.compiled;
      "purescript:${projectName}:node-modules" = purs.nodeModules;
      "purescript:${projectName}:bundle" = purs.bundlePursProject { main = "Test.Main"; entrypoint = "app/index.js"; bundledModuleName = "dist/output.js"; };
      "purescript:${projectName}:docs" = purs.buildPursDocs { };
      "purescript:${projectName}:docs-search" = purs.buildSearchablePursDocs { };
    };

    checks = {
      "purescript:${projectName}:check" = purs.runPursTest { };
    };

    devShell = purs.devShell;
  };
in
mkFlake pursProjOpts.projectName (pkgs.purescriptProject pursProjOpts)
