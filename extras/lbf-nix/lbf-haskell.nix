# Base API for constructing Haskell Cabal packages given .lbf schemas

# Nixpkgs
pkgs:
# LambdaBuffers Frontend
lbf:
# LambdaBuffers Haskell Codegen
lbg-haskell:
let
  lbfHaskellOpts =
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
      # Examples: version = "1.1.0.0"
      version ? "1.1.0.0"
    }: { inherit src imports files classes dependencies configs name version; };

  lbf-build = import ./lbf-build.nix pkgs lbf;

  lbfBuild = opts: with (lbfHaskellOpts opts);
    lbf-build.build
      {
        inherit src;
        opts = {
          inherit files;
          import-paths = imports;
          gen = lbg-haskell;
          gen-classes = classes;
          gen-dir = "autogen";
          gen-opts = builtins.map (c: "--config=${c}") configs; # WARN(bladyjoker): If I put quotes here everything breaks.
          work-dir = ".work";
        };
      };

  cabalTemplate = opts: with (lbfHaskellOpts opts);
    pkgs.writeTextFile {
      name = "lambda-buffers-cabal-template";
      text = ''
        cabal-version:      3.0
        name:               ${name}
        version:            ${version}
        synopsis:           A Cabal project that contains LambdaBuffers generated Haskell modules
        build-type:         Simple

        library
            exposed-modules: <EXPOSED_MODULES>
            autogen-modules: <EXPOSED_MODULES>
            hs-source-dirs:     autogen

            default-language: Haskell2010
            default-extensions: NoImplicitPrelude
            build-depends: <DEPS>
      '';
    };

  build = opts: with (lbfHaskellOpts opts);
    let
      lbfBuilt = lbfBuild opts;
    in
    pkgs.stdenv.mkDerivation {
      inherit src version;
      pname = name;
      outputs = [ "out" "buildjson" ];
      buildInputs = [
        pkgs.cabal-install
        pkgs.jq
      ];
      buildPhase = ''
        set -vox;
        ln -s ${lbfBuilt} autogen;
        ln -s ${lbfBuilt.workdir} .work-dir;
        ln -s ${lbfBuilt.buildjson} build.json;

        EXPOSED_MODULES=$(find autogen/ -name "*.hs" | while read f; do grep -Eo 'module\s+\S+\s+' $f | head -n 1 | sed -r 's/module\s+//' | sed -r 's/\s+//'; done | tr '\n' ' ');
        echo "Found generated modules $EXPOSED_MODULES";

        DEPS=$(echo ${builtins.concatStringsSep " " dependencies} $(cat build.json | jq -r ".[]" | sort -u) | sed 's/ /, /g');
        echo "Gathered Cabal deps $DEPS";
        cat ${cabalTemplate opts} \
            | sed -r "s/<EXPOSED_MODULES>/$EXPOSED_MODULES/" \
            | sed -r "s/<DEPS>/$DEPS/" > ${name}.cabal;
      '';

      installPhase = ''
        mkdir -p $out;
        cp -rL autogen $out
        cp ${name}.cabal $out/${name}.cabal;
        find $out;

        cp build.json $buildjson;
        cat $buildjson
      '';
    };
in
build
