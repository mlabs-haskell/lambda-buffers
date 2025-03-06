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
      src,
      # Additional sources that are passed to `lbf` as the `--import-path` flag.
      # Examples: imports = { lbf-prelude = ./lbf-prelude; }
      imports ? { },
      # .lbf files in `src` to compile and codegen.
      # Examples: files = [ "Foo.lbf" "Foo/Bar.lbf" ]
      files,
      # Classes for which to generate implementations for (default lbf-prelude classes).
      classes ? [ ],
      # Dependencies to include in the Cargo's `dependencies` section.
      # examples: dependencies = [ "lbf-prelude" ]
      dependencies ? [ ],
      configs ? [ ],
      # Name of the package and also the name of the Cargo crate.
      # Examples: name = "lbf-myproject"
      name,
      # Version of the package and also the version of the Cargo crate.
      # Examples: version = "0.1.0.0"
      version ? "0.1.0",
      # Version of dependencies
      # A package will only be added to Cargo.toml, if the generated code directly depends on it
      # Defaults to version 0.1.0 for all packages
      extraVersions ? { },
    }:
    {
      inherit
        src
        imports
        files
        classes
        dependencies
        configs
        name
        version
        extraVersions
        ;
    };

  lbf-build = import ./lbf-build.nix pkgs lbf;

  lbfBuild =
    opts:
    with (lbfRustOpts opts);
    let
      findModules =
        root:
        map (
          path:
          builtins.replaceStrings [ "/" ] [ "." ] (
            pkgs.lib.strings.removePrefix "./" (
              pkgs.lib.strings.removeSuffix ".lbf" (pkgs.lib.path.removePrefix root path)
            )
          )
        ) (builtins.filter (pkgs.lib.hasSuffix ".lbf") (pkgs.lib.filesystem.listFilesRecursive root));
      packageSet = pkgs.writeTextFile {
        name = "lb-packages";
        text = builtins.toJSON ({ crate = findModules src; } // builtins.mapAttrs (_: findModules) imports);
      };

    in
    lbf-build.build {
      inherit src;
      opts = {
        inherit files;
        import-paths = pkgs.lib.attrsets.attrValues imports;
        gen = lbg-rust;
        gen-classes = classes;
        gen-dir = "autogen";
        gen-opts = [ "--packages=${packageSet}" ] ++ builtins.map (c: "--config=${c}") configs; # WARN(bladyjoker): If I put quotes here everything breaks.
        work-dir = ".work";
      };
    };

  cargoTemplate =
    opts:
    with (lbfRustOpts opts);
    pkgs.writers.writeJSON "lambda-buffers-cargo-template" {
      package = {
        inherit name version;
        edition = "2021";
      };
    };

  # This is a lookup table of default crate versions used by lamba-buffers modules
  # Based on the contents of `build.json` a subset of these will be attached to the
  # Cargo.toml file
  # Note: lbr-prelude and and plutus prelude versions are pinned here, but can be overridden with `extraVersions`
  versionTable = {
    num-bigint = "~0.4";
    serde_json = {
      version = "^1.0";
      features = [ "arbitrary_precision" ];
    };
    plutus-ledger-api = {
      version = "^3.0.1";
      features = [ "lbf" ];
    };
    lbr-prelude = {
      version = "0.1.3";
    };
  };

  crateVersions =
    opts:
    with (lbfRustOpts opts);
    pkgs.writers.writeJSON "lambda-buffers-crate-versions" (versionTable // extraVersions);

  build =
    opts:
    with (lbfRustOpts opts);
    let
      lbfBuilt = lbfBuild opts;
    in
    pkgs.stdenv.mkDerivation {
      inherit src version;
      pname = name;
      outputs = [
        "out"
        "buildjson"
      ];
      buildInputs = [
        pkgs.jq
        pkgs.yj
      ];
      buildPhase = ''
        ln -s ${lbfBuilt} autogen;
        ln -s ${lbfBuilt.workdir} .work-dir;
        ln -s ${lbfBuilt.buildjson} build.json;

        # Generating Cargo manifest file
        DEPS=$(echo ${builtins.concatStringsSep " " dependencies} $(cat build.json | jq -r ".[]" | sort -u));
        echo "Gathered Cargo deps $DEPS";
        cat ${cargoTemplate opts} > Cargo.json;
        # Using the lookup table `crateVersions`, filling in the library version.
        # If no version is found, we default to a local path dependency, pointing to
        # a sibling directory (directory in extra-sources or .extras)
        # e.g.: for `lbr-prelude` we print `lbr-prelude = { path = "../lbr-prelude" }
        for DEP in $DEPS; do
          if [ $DEP != "std" ]; then
            VER=$(cat ${crateVersions opts} | jq ".\"$DEP\"" -c);
            if [ $VER == "null" ]; then
              VER="{\"path\": \"../$DEP-0.1.0\"}"
            fi
            cat Cargo.json | jq ".dependencies+={\"$DEP\":$VER}" > tmp.json;
            mv tmp.json Cargo.json
          fi
        done
        cat Cargo.json | yj -jt > Cargo.toml;
      '';

      installPhase = ''
        cp build.json $buildjson;
        echo "Dependencies collected"

        mkdir -p $out/src;
        cp -r autogen/* $out/src
        cp Cargo.toml $out/Cargo.toml;

        # Generating module files
        chmod -R u+w $out/src
        pushd $out/src

        # Collecting modules of the library and attaching a module declaration
        # to parent modules. Any directory in the path must also
        # be considered as a module (e.g. for `foo/bar/baz.rs` we have to create
        # `lib.rs`, `foo.rs`and `foo/bar.rs`)
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
