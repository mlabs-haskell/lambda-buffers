# Base API for constructing Rust packages given .lbf schemas

# Nixpkgs
pkgs:
# LambdaBuffers Frontend
lbf:
# LambdaBuffers Rust Codegen
lbg-rust:
let
  lbfRustOpts =
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
      version ? "0.1.0"
    }: { inherit src imports files classes dependencies configs name version; };

  lbf-build = import ./lbf-build.nix pkgs lbf;

  lbfBuild = opts: with (lbfRustOpts opts);
    lbf-build.build
      {
        inherit src;
        opts = {
          inherit files;
          import-paths = imports;
          gen = lbg-rust;
          gen-classes = classes;
          gen-dir = "autogen";
          gen-opts = builtins.map (c: "--config=${c}") configs; # WARN(bladyjoker): If I put quotes here everything breaks.
          work-dir = ".work";
        };
      };

  cargoTemplate = opts: with (lbfRustOpts opts);
    pkgs.writeTextFile {
      name = "lambda-buffers-cabal-template";
      text = ''
        [package]
        name = "${name}"
        version = "${version}"
        edition = "2021"

        [dependencies]
      '';
    };

  crateVersions = pkgs.writeTextFile {
    name = "lambda-buffers-crate-versions";
    text = ''
      num-bigint = "0.4.4"
      serde_json = { version = "1.0.107", features = ["arbitrary_precision"] }
      plutus-ledger-api = { github = "https://github.com/mlabs-haskell/plutus-ledger-api-rust", features = ["lbf"] }
      lbr-prelude = { path = "../lbr-prelude" }
      lbr-prelude-derive = { path = "../lbr-prelude-derive" }
    '';
  };

  build = opts: with (lbfRustOpts opts);
    let
      lbfBuilt = lbfBuild opts;
    in
    pkgs.stdenv.mkDerivation {
      inherit src version;
      pname = name;
      outputs = [ "out" "buildjson" ];
      buildInputs = [
        pkgs.jq
      ];
      buildPhase = ''
        ln -s ${lbfBuilt} autogen;
        ln -s ${lbfBuilt.workdir} .work-dir;
        ln -s ${lbfBuilt.buildjson} build.json;

        # Generating Cargo manifest file
        DEPS=$(echo ${builtins.concatStringsSep " " dependencies} $(cat build.json | jq -r ".[]" | sort -u));
        echo "Gathered Cargo deps $DEPS";
        cat ${cargoTemplate opts} > Cargo.toml;
        for DEP in $DEPS; do
          if [ $DEP != "std" ]; then
            echo "$(cat ${crateVersions} | grep "$DEP" || echo "$DEP = { path = \"../$DEP\" }")" >> Cargo.toml
          fi
        done
      '';

      installPhase = ''
        cp build.json $buildjson;
        echo "Dependencies collected"
        cat $buildjson;

        mkdir -p $out/src;
        cp -r autogen/* $out/src
        cp Cargo.toml $out/Cargo.toml;

        # Generating module files
        chmod -R u+w $out/src
        pushd $out/src

        MODS=$(find . -type f -name "*.rs")
        MODS+=" "
        MODS+=$(find . -type d)

        for MOD in $MODS; do
          if [ "$MOD" != "." ]; then
            if [ $(dirname $MOD) = "." ];
              then MODFILE="lib.rs";
              else MODFILE=$(dirname $MOD).rs;
            fi
            DOC="pub mod $(basename $MOD .rs);"

            if [ ! $(grep "$DOC" $MODFILE) ]; then
              echo $DOC >> $MODFILE;
            fi
          fi
        done

        echo "Files generated"
        find $out/;
      '';
    };
in
build
