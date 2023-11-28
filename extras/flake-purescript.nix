pkgs: pursProjOpts:
let
  mkFlake = projectName: purs:
    {
      packages = {
        "purescript:${projectName}:src" = pkgs.stdenv.mkDerivation {
          name = projectName;
          inherit (pursProjOpts) src;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };
        "purescript:${projectName}:lib" = purs.compiled;
        "purescript:${projectName}:node-modules" = purs.nodeModules;
        "purescript:${projectName}:webpack-web" = purs.bundlePursProjectWebpack {
          main = "Test.Main";
          entrypoint = "app/index.js";
          bundledModuleName = "dist/output.js";
        };
        "purescript:${projectName}:esbuild-web" = purs.bundlePursProjectEsbuild {
          main = "Test.Main";
          browserRuntime = true;
        };
        "purescript:${projectName}:esbuild-nodejs" = purs.bundlePursProjectEsbuild {
          main = "Test.Main";
          browserRuntime = false;
        };

        "purescript:${projectName}:docs" = purs.buildPursDocs { };
        "purescript:${projectName}:docs-search" = purs.buildSearchablePursDocs { };
      };

      checks = {
        "purescript:${projectName}:check-nodejs" = purs.runPursTest {
          testMain = "Test.Main";
          buildInputs = if pursProjOpts ? "shell" then if pursProjOpts.shell ? "packages" then pursProjOpts.shell.packages else [ ] else [ ];
        };
      };

      devShell = purs.devShell;
    };
in
mkFlake pursProjOpts.projectName (pkgs.purescriptProject pursProjOpts)
