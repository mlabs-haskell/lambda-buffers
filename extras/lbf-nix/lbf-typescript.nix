# Base API for constructing Typescript packages given .lbf schemas
# Warning(jaredponn): Essentially duplicated code from `./lbf-purescript.nix`

# Nixpkgs
pkgs:
# LambdaBuffers Frontend
lbf:
# LambdaBuffers Typescript Codegen
lbg-typescript:
let
  lbfTypescriptOpts =
    {
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
    , # Dependencies to include in the Cabal's `build-depends` stanza.
      # examples: dependencies = [ "lbf-prelude" ]
      dependencies ? [ ]
    , configs ? [ ]
    , # Name of the package and also the name of the Cabal package.
      # Examples: name = "lbf-myproject"
      name
    , # Version of the package and also the version of the Cabal package.
      # Examples: version = "0.1.0.0"
      version ? "0.1.0.0"
    }: { inherit src imports files classes dependencies configs name version; };

  lbf-build = import ./lbf-build.nix pkgs lbf;

  lbfBuild = opts: with (lbfTypescriptOpts opts);
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

  packageJsonTemplate = opts: with lbfTypescriptOpts opts;
    pkgs.writeTextFile {
      name = "lambda-buffers-package-json-template";
      text =
        ''
          {
            "name": "${name}",
            "version": "${version}",
            "description": "A package.json that contains LambdaBuffers generated TypesScript files",
            "lockfileVersion": 2,
            "exports": {
              ".": "./dist/LambdaBuffers/PATH_TO_AUTO_GENERATED_FILE.js",
              "./package.json": "./package.json"
            },
            "type": "module",
            "scripts": {
              "build": "npx tsc src",
              "test": ":"
            },
            "devDependencies": {
              "typescript": "^5.2.2"
            },
            "files": ["./dist/LambdaBuffers/**/*"],
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
            "noUnusedLocals": true, 
            "noUnusedParameters": true,
            "exactOptionalPropertyTypes": true,
            "noImplicitReturns": true,
            "noFallthroughCasesInSwitch": true,
            "noUncheckedIndexedAccess": true,
            "noImplicitOverride": true,
            "noPropertyAccessFromIndexSignature": true,
          }
        }
      '';
  };

  build = opts: with (lbfTypescriptOpts opts);
    let
      lbfBuilt = lbfBuild opts;
    in
    pkgs.stdenv.mkDerivation {
      inherit src version;
      pname = name;
      outputs = [ "out" "buildjson" ];
      buildInputs = [
        pkgs.jq
        pkgs.nodejs-18_x
      ];
      buildPhase = ''
        ln -s ${lbfBuilt} autogen;
        ln -s ${lbfBuilt.workdir} .work-dir;
        ln -s ${lbfBuilt.buildjson} build.json;

        ###########################
        # Copying build outputs
        ###########################
        cp build.json $buildjson;
        echo "Dependencies collected: $buildjson"

        mkdir -p $out/src;
        cp -r autogen/* $out/src

        ###########################
        # Creating the package.json
        ###########################
        cd $out
        echo 'Creating `package.json`'
        LB_GENERATED_FILE=$(find src -type f)
        DIST_LB_GENERATED_FILE="./dist/$LB_GENERATED_FILE"

        echo "LambdaBuffers generated: $LB_GENERATED_FILE..."

        NUMBER_OF_GENERATED_FILES=$(wc -l << EOF
            "$LB_GENERATED_FILE"
        EOF
        )

        if test "$NUMBER_OF_GENERATED_FILES" -ne "1"
            then  
                echo 'Error: LambdaBuffers should generate exactly one file'
                exit 1
        fi

        JQ_FILTER=".\"exports\".\".\"=\"$DIST_LB_GENERATED_FILE\""

        cat ${packageJsonTemplate opts} | jq  $JQ_FILTER > $out/package.json

        ###########################
        # Creating the tsconfig.json
        ###########################
        cd $out
        cat ${tsConfigJson} > $out/tsconfig.json

        ###########################
        # Build the Typescript project
        ###########################
        npm run build

        npm pack
      '';
      installPhase = ''
      '';
    };
in
build
