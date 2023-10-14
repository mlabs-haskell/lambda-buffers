# `lbf build` Nix API
# Code hints: `builtOpts` is a 'data constructor' which is then passed to 'buildCall' that prints the shell line and 'build' that produces the derivation.

# Nixpkgs
pkgs:
# LambdaBuffers Frontend CLI path.
lbf:
let
  utils = import ./utils.nix pkgs;

  buildOpts =
    {
      # .lbf files to compile and codegen that are passed to `lbf` as last positional arguments.
      # Examples: files = [ "Foo.lbf" "Foo/Bar.lbf" ]
      files
    , # Additional sources that are passed to `lbf` as the `--import-path` flag.
      # Examples: imports-paths = [ ./. lbf-prelude ]
      import-paths ? [ ]
    , # Codegen to use to generate files that is passed to `lbf` as the `--gen` flag.
      gen
    , # Classes for which to generate implementations for that are passed to `lbf` as the `--gen-class` flag.
      gen-classes ? [ ]
    , # Codegen options to pass via `lbf` using the `--gen-opt` flag.
      gen-opts ? [ ]
    , # Codegen output directory that is passed to `lbf` as the `--gen-dir` flag.
      gen-dir ? ".autogen"
    , # Working directory used by `lbf` and passed as the `--work-dir` flag.
      work-dir ? ".work"
    }: { inherit files import-paths gen gen-classes gen-opts gen-dir work-dir; };

  buildCall = opts: with (buildOpts opts);
    builtins.concatStringsSep " "
      [
        "${lbf}/bin/lbf"
        "build"
        (utils.mkFlags "import-path" import-paths)
        (utils.mkFlag "work-dir" work-dir)
        (utils.mkFlag "gen" gen)
        (utils.mkFlags "gen-class" gen-classes)
        (utils.mkFlags "gen-opt" gen-opts)
        (utils.mkFlag "gen-dir" gen-dir)
        (builtins.concatStringsSep " " files)
      ];

  build = { src, opts }: with (buildOpts opts);
    pkgs.stdenv.mkDerivation {
      inherit src;
      name = "lbf-build";
      outputs = [ "out" "buildjson" "workdir" ];
      buildPhase = ''
        for f in ${builtins.concatStringsSep " " opts.import-paths}; do find $f; done;
        for f in ${builtins.concatStringsSep " " import-paths}; do find $f; done;
        mkdir ${gen-dir};
        mkdir ${work-dir};
        find .
        export LC_CTYPE=C.UTF-8
        export LC_ALL=C.UTF-8
        export LANG=C.UTF-8
        echo ${buildCall opts};
        ${buildCall opts};
      '';

      installPhase = ''
        mv ${gen-dir}/build.json $buildjson;
        cat $buildjson

        mkdir $out;
        mv ${gen-dir}/* $out;
        find $out

        mkdir $workdir;
        mv ${work-dir}/* $workdir;
        find $workdir
      '';
    };
in
{ inherit buildOpts build; }
