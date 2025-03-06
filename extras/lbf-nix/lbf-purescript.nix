# Base API for constructing Purescript packages given .lbf schemas

# Nixpkgs
pkgs:
# LambdaBuffers Frontend
lbf:
# LambdaBuffers Purescript Codegen
lbg-purescript:
let
  lbfPurescriptOpts =
    {
      # Source that is passed to `lbf` as the `--import-path` flag and used to find `files`.
      # Examples: src = ./api
      src,
      # Additional sources that are passed to `lbf` as the `--import-path` flag.
      # Examples: imports = [ lbf-prelude ]
      imports ? [ ],
      # .lbf files in `src` to compile and codegen.
      # Examples: files = [ "Foo.lbf" "Foo/Bar.lbf" ]
      files,
      # Classes for which to generate implementations for (default lbf-prelude classes).
      classes ? [ ],
      # Dependencies to include in the Cabal's `build-depends` stanza.
      # examples: dependencies = [ "lbf-prelude" ]
      dependencies ? [ ],
      configs ? [ ],
      # Name of the package and also the name of the Cabal package.
      # Examples: name = "lbf-myproject"
      name,
      # Version of the package and also the version of the Cabal package.
      # Examples: version = "0.1.0.0"
      version ? "0.1.0.0",
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
        ;
    };

  lbf-build = import ./lbf-build.nix pkgs lbf;

  lbfBuild =
    opts:
    with (lbfPurescriptOpts opts);
    lbf-build.build {
      inherit src;
      opts = {
        inherit files;
        import-paths = imports;
        gen = lbg-purescript;
        gen-classes = classes;
        gen-dir = "autogen";
        gen-opts = builtins.map (c: "--config=${c}") configs; # WARN(bladyjoker): If I put quotes here everything breaks.
        work-dir = ".work";
      };
    };

  build =
    opts:
    with (lbfPurescriptOpts opts);
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
        pkgs.cabal-install
        pkgs.jq
      ];
      buildPhase = ''
        ln -s ${lbfBuilt} autogen;
        ln -s ${lbfBuilt.workdir} .work-dir;
        ln -s ${lbfBuilt.buildjson} build.json;
      '';

      installPhase = ''
        cp build.json $buildjson;
        echo "Dependencies collected"
        cat $buildjson;

        mkdir -p $out/src;
        cp -r autogen/* $out/src
        echo "Files generated"
        find $out/;
      '';
    };
in
build
