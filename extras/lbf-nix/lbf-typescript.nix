# Base API for constructing Typescript packages given .lbf schemas

# Nixpkgs
pkgs:
# LambdaBuffers Frontend
lbf:
# LambdaBuffers Typescript Codegen
lbg-typescript:

# Options
lbfTypescriptOpts@{
  # Source that is passed to `lbf` as the `--import-path` flag and used to find `files`.
  # Examples: src = ./api
  src
, # Additional sources that are passed to `lbf` as the `--import-path` flag.
  # Examples: imports = [ lbf-prelude ]
  imports ? [ ]
, # .lbf files in `src` to compile and codegen.
  # Examples: files = [ "Foo.lbf" "Foo/Bar.lbf" ]
  files
  # Classes for which to generate implementations for (default lbf-prelude classes).
, classes ? [ ]
, # npmDependencies are tarballs from `npm pack` of the dependencies required
  npmDependencies ? [ ]
  # , # npmDevDependencies are tarballs from `npm pack` of the develoepr dependencies for building the project
  #   # TODO: actually use this
  #   npmDevDependencies ? [ ]
, configs ? [ ]
, # Name of the package and also the name of the Cabal package.
  # Examples: name = "lbf-myproject"
  name
, # Version of the package and also the version of the Cabal package.
  # Examples: version = "0.1.0.0"
  version ? "0.1.0.0"

, projectTarballHash ? pkgs.lib.fakeHash
}:
let
  lbf-build = import ./lbf-build.nix pkgs lbf;

  lbfBuilt = with lbfTypescriptOpts;
    lbf-build.build
      {
        inherit src;
        opts = {
          inherit files;
          import-paths = imports;
          gen = lbg-typescript;
          gen-classes = classes;
          gen-dir = "autogen";
          gen-opts = builtins.map (c: "--config=${c}") configs; # WARN(bladyjoker): If I put quotes here everything breaks.
          work-dir = ".work";
        };
      };

  packageJsonTemplate = with lbfTypescriptOpts;
    pkgs.writeTextFile {
      name = "lambda-buffers-package-json-template";
      text =
        ''
          {
            "name": "${name}",
            "version": "${version}",
            "description": "A package.json that contains LambdaBuffers generated TypeScript files",
            "lockfileVersion": 2,
            "exports": {
              ".": "./dist/LambdaBuffers/PATH_TO_AUTO_GENERATED_FILE.js",
              "./package.json": "./package.json"
            },
            "type": "module",
            "scripts": {
              "build": "tsc -b .",
              "test": ":"
            },
            "devDependencies": {
            },
            "files": ["./dist/LambdaBuffers/**/*" ],
            "dependencies": {}
          }
        '';
    };

  # TODO(jaredponn): allow this to be passed in as a parameter
  tsConfigJson = pkgs.writeTextFile {
    name = "lambda-buffers-tsconfig-json-template";
    text =
      ''
        {
          "compilerOptions": {
            "target": "es2020", 
            "module": "node16", 
            "moduleResolution": "node16",
            "rootDir": "./src",
            "declaration": true, 
            "declarationMap": true, 
            "sourceMap": true,
            "outDir": "./dist",
            "verbatimModuleSyntax": true, 
            "forceConsistentCasingInFileNames": true,

            /* Type Checking */
            "strict": true,
            "noImplicitAny": true,
            "strictNullChecks": true,
            "strictFunctionTypes": true,
            "strictBindCallApply": true,
            "strictPropertyInitialization": true,
            "noImplicitThis": true,
            "useUnknownInCatchVariables": true,
            "alwaysStrict": true,
            "exactOptionalPropertyTypes": true,
            "noUncheckedIndexedAccess": true,
            "noImplicitOverride": true,
            "noPropertyAccessFromIndexSignature": true,
            "noImplicitReturns": true,
            "noFallthroughCasesInSwitch": true,
            "noUnusedLocals": true, 
            "noUnusedParameters": true,
          }
        }
      '';
  };

  # The source files + a template of the package.json + the Typescript config
  # TODO(jaredponn): is there some way we can combine this with
  # `lbf-nix.typescriptFlake`?

  #   https://github.com/NixOS/nixpkgs/tree/master/pkgs/build-support/node/build-npm-package

  npmLocalDependencies = pkgs.callPackage (import ../typescript/npm-local-dependencies.nix) { } { inherit name npmDependencies; };

  lbTypescriptNpmProject = with lbfTypescriptOpts;
    pkgs.stdenv.mkDerivation {
      inherit src version;
      pname = name;
      # outputs = [ "out" "buildjson" ];
      buildInputs = [
        pkgs.jq
        pkgs.nodejs-18_x
        pkgs.typescript
      ];
      buildPhase =
        ''
          export HOME=$(mktemp -d)
          # TODO(jaredponn): remove these symlinks
          # ln -s ${lbfBuilt} autogen;
          # ln -s ${lbfBuilt.workdir} .work-dir;
          # ln -s ${lbfBuilt.buildjson} build.json;

          # mkdir -p $out/src;
          # cp -r autogen/* $out/src


          mkdir src
          cp -r ${lbfBuilt}/* src

          ###########################
          # Creating the package.json
          ###########################
          echo 'Creating `package.json`'

          # Change directory to src so we don't include `src` in the output path
          # AND we rename the file so it is the actual file generated by
          # TypeScript
          LB_GENERATED_FILE=$(cd src && realpath -m --relative-to=. "$(find . -type f)" )
          DIST_LB_GENERATED_FILE=$(echo "./dist/$LB_GENERATED_FILE" | sed 's/\.mts$/.mjs/')

          echo "LambdaBuffers generated: $LB_GENERATED_FILE..."

          # Quickly test if there is exactly one file
          # [should never happen]
          if test "$( { cd src && find . -type f ; } | wc -l)" -ne "1"
              then  
                  echo 'Error: LambdaBuffers should generate exactly one file'
                  exit 1
          fi

          JQ_FILTER=".\"exports\".\".\"=\"$DIST_LB_GENERATED_FILE\""

          cat ${packageJsonTemplate} | ${pkgs.jq}/bin/jq $JQ_FILTER > package.json

          ###########################
          # Symlink dependencies
          ###########################
          ${npmLocalDependencies.npmLocalDependenciesLinkCommand}

          ###########################
          # Creating the tsconfig.json
          ###########################
          cat ${tsConfigJson} > tsconfig.json

          ###########################
          # Install all the dependencies
          ###########################
          npm install --save ${npmLocalDependencies.npmLocalDependenciesFolder}/*.tgz

          ###########################
          # Build the project
          ###########################
          npm run build

          # It's nice to include the generated file in the output
          cp -r ${lbfBuilt}/* dist/
        '';
    };

  tgz = lbTypescriptNpmProject.overrideAttrs (_self: super:
    {
      outputHash = projectTarballHash;
      outputHashMode = "flat";

      name = "${name}.tgz";
      makeCacheWritable = true;
      installPhase =
        ''
          tgzFile=$(npm --log-level=verbose pack | tail -n 1)
          mv $tgzFile $out
        '';
    });

in
tgz
