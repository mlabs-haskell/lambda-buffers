# Base API for constructing Typescript packages given .lbf schemas
{
  # Nixpkgs
  pkgs
  # LambdaBuffers Frontend
, lbf
  # LambdaBuffers Typescript Codegen
, lbg-typescript
  # Function to create typescript flakes
, typescriptFlake
}:


# Options
lbfTypescriptOpts@{
  # Source that is passed to `lbf` as the `--import-path` flag and used to find `files`.
  # Examples: src = ./api
  src
, # Additional sources that are passed to `lbf` as the `--import-path` flag
  # Examples: imports = { "lbf-prelude" = "./lbf-prelude"; "my-package-name" = "./path/to/my-package-name"; }
  imports ? { }
, # .lbf files in `src` to compile and codegen.
  # Examples: files = [ "Foo.lbf" "Foo/Bar.lbf" ]
  files
  # Classes for which to generate implementations for (default lbf-prelude classes).
, classes ? [ ]
, # npmExtraDependencies are tarballs from `npm pack` of the dependencies required
  npmExtraDependencies ? [ ]
  # , # npmDevDependencies are tarballs from `npm pack` of the develoepr dependencies for building the project
  #   # TODO(jaredponn): actually use this
  #   npmDevDependencies ? [ ]
, configs ? [ ]
, # Name of the package and also the name of the Cabal package.
  # Examples: name = "lbf-myproject"
  name
, # Version of the package and also the version of the Cabal package.
  # Examples: version = "0.1.0.0"
  version ? "1.0.0"

, # `tsconfig.json` for TypeScript. Note that the typechecking options do
  # nothing as TypeScript's semantic checking is disabled.
  tsConfigJson ? pkgs.writeTextFile {
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
  }

}:
let
  lbf-build = import ./lbf-build.nix pkgs lbf;

  # TODO(jaredponn): this was (essentially) stolen from the Rust side. This
  # should be made a common function.
  findModules = root: builtins.map
    (path: builtins.replaceStrings [ "/" ] [ "." ]
      (pkgs.lib.strings.removePrefix "./" (pkgs.lib.strings.removeSuffix ".lbf"
        (pkgs.lib.strings.removePrefix root path))))
    (builtins.filter (pkgs.lib.hasSuffix ".lbf")
      (pkgs.lib.filesystem.listFilesRecursive root));

  packageSet =
    pkgs.writeTextFile {
      name = "lb-typescript-packages";
      text =
        builtins.toJSON
          (builtins.mapAttrs (_name: findModules) imports);
    };

  lbfBuilt = with lbfTypescriptOpts;
    lbf-build.build
      {
        inherit src;
        opts = {
          inherit files;
          import-paths = pkgs.lib.attrsets.attrValues imports;
          gen = lbg-typescript;
          gen-classes = classes;
          gen-dir = "autogen";
          gen-opts =
            [ "--packages=${packageSet}" ]
            ++ builtins.map (c: "--config=${c}") configs;
          # WARN(bladyjoker): If I put quotes here everything breaks.
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
            "dependencies": {
            }
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


  # `lbTypescriptSrc` is the Typescript generated code from LambdaBuffers
  # Notes:
  #     - the `package.json` and `package-lock.json` are still missing
  #     dependencies and needs to be added in a later step
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

          # TODO(jaredponn): LambdaBuffers module names should not have "weird"
          # characters. Probably should write a quick check for this..

          # Note(jaredponn):
          # What is going on here? We create a JQ filter which 
          # - For every `LambdaBuffers/Module/Generated.mts`,
          # - Add `exports."./LambdaBuffers/Module/Generated.mts": "./dist/LambdaBuffers/Module/Generated.mts"` to `package.json`

          # `.` is the identity jq filter
          echo "." > jq_filter.jq
          find autogen/ -type f -iname '*.mts' \
            | xargs -I % sh -c 'echo "$0" | sed -E "s#^autogen/(.*)\.mts\$#\1.mjs#"' % \
            | xargs -I % sh -c 'echo "| .\"exports\".\"./$0\"=\"./dist/$0\"" >> jq_filter.jq' %

          cat ${packageJsonTemplate} | jq -f jq_filter.jq > package.json
          cat ${packageJsonLockTemplate} > package-lock.json
          cat ${tsConfigJson} > tsconfig.json

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
        '';
    };


  lbTypescriptFlake =
    (typescriptFlake {
      inherit (lbfTypescriptOpts) name npmExtraDependencies;
      src = lbTypescriptSrc;
    }).extend (_self: super: {
      __typescriptFlake__ = super.__typescriptFlake__.extend (tsSelf: tsSuper: {
        srcWithNode2nix = tsSuper.srcWithNode2nix.overrideAttrs
          (_self: _super:
            {
              # Beef up the postConfigure so we add the extra
              # dependencies to the `package.json` / `package-lock.json`
              postConfigure =
                ''
                  export HOME=$(mktemp -d)
                  npm install \
                      --loglevel verbose \
                      --offline \
                      --package-lock-only \
                      --save ./${tsSelf.npmExtraDependenciesFolder}/*
                '';
            }
          );
      });
    });
in
lbTypescriptFlake
