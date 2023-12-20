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
  version ? "1.0.0"

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
              "build": "npx tsc -b .",
              "test": ":"
            },
            "devDependencies": {
            },
            "files": ["./dist/LambdaBuffers/**/*" ],
            "dependencies": {}
          }
        '';
    };

  # TODO(jaredponn): urgh, this was manually copy pastad in by hand.. automate
  # this more nicely later... there's a huge pain point with how this needs to
  # hit the network to create the package-lock.json...
  packageJsonLockTemplate = with lbfTypescriptOpts;
    pkgs.writeTextFile {
      name = "lambda-buffers-package-lock-template";
      text =
        ''
          {
            "name": "${name}",
            "version": "${version}",
            "lockfileVersion": 2,
            "requires": true,
            "packages": {
              "": {
                "name": "${name}",
                "version": "${version}",
                "license": "ISC",
                "devDependencies": {
                  "typescript": "^5.3.3"
                }
              },
              "node_modules/typescript": {
                "version": "5.3.3",
                "resolved": "https://registry.npmjs.org/typescript/-/typescript-5.3.3.tgz",
                "integrity": "sha512-pXWcraxM0uxAS+tN0AG/BF2TyqmHO014Z070UsJ+pFvYuRSq8KH8DmWpnbXe0pEPDHXZV3FcAbJkijJ5oNEnWw==",
                "dev": true,
                "bin": {
                  "tsc": "bin/tsc",
                  "tsserver": "bin/tsserver"
                },
                "engines": {
                  "node": ">=14.17"
                }
              }
            },
            "dependencies": {
              "typescript": {
                "version": "5.3.3",
                "resolved": "https://registry.npmjs.org/typescript/-/typescript-5.3.3.tgz",
                "integrity": "sha512-pXWcraxM0uxAS+tN0AG/BF2TyqmHO014Z070UsJ+pFvYuRSq8KH8DmWpnbXe0pEPDHXZV3FcAbJkijJ5oNEnWw==",
                "dev": true
              }
            }
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

  # ${npmLocalDependencies.npmLocalDependenciesLinkCommand}

  lbTypescriptSrc = with lbfTypescriptOpts;
    pkgs.stdenv.mkDerivation {
      inherit src version;
      pname = name;
      outputs = [ "out" "buildjson" ];
      buildInputs = [
        pkgs.jq
        pkgs.nodejs-18_x
      ];
      buildPhase =
        ''
          export HOME=$(mktemp -d)
          # TODO(jaredponn): remove these symlinks
          ln -s ${lbfBuilt} autogen;
          ln -s ${lbfBuilt.workdir} .work-dir;
          ln -s ${lbfBuilt.buildjson} build.json;


          ###########################
          # Creating the packages json files
          ###########################
          echo 'Creating `package.json`, `package-lock.json`, `tsconfig.json`'

          # Change directory to autogen so we don't include `autogen` in the output path
          # AND we rename the file so it is the actual file generated by
          # TypeScript
          LB_GENERATED_FILE=$(cd autogen && realpath -m --relative-to=. "$(find . -type f)" )
          DIST_LB_GENERATED_FILE=$(echo "./dist/$LB_GENERATED_FILE" | sed 's/\.mts$/.mjs/')

          echo "LambdaBuffers generated: $LB_GENERATED_FILE..."

          # TODO(jaredponn): okay we should really allow having multiple
          # modules to be be generated
          if test "$( { cd autogen && find . -type f ; } | wc -l)" -ne "1"
              then  
                  echo 'TODO(jaredponn): Please report a bug -- TypeScript nix facilities only support LambdaBuffers to generate exactly one file for now'
                  exit 1
          fi

          JQ_FILTER=".\"exports\".\".\"=\"$DIST_LB_GENERATED_FILE\""

          cat ${packageJsonTemplate} | jq $JQ_FILTER > package.json
          cat ${packageJsonLockTemplate} > package-lock.json
          cat ${tsConfigJson} > tsconfig.json

          ###########################
          # Symlink dependencies
          ###########################
          ${npmLocalDependencies.npmLocalDependenciesLinkCommand}
        '';

      installPhase =
        ''
          # buildjson output
          cp build.json $buildjson

          # out output
          mkdir -p $out/src
          cp -r autogen/* $out/src

          cp ./package.json $out
          cp ./package-lock.json $out
          cp ./tsconfig.json $out
          cp -r ${npmLocalDependencies.npmLocalDependenciesFolder} $out

          cd $out
          npm install --offline --package-lock-only -save ${npmLocalDependencies.npmLocalDependenciesFolder}/*
        '';
    };


  lbTypescriptFlake = with lbfTypescriptOpts;
    import ../flake-typescript.nix pkgs {
      inherit name npmDependencies;
      src = lbTypescriptSrc;
    };

  # in lbTypescriptSrc
in
lbTypescriptFlake 
