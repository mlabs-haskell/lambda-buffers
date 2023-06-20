# LambdaBuffers Frontend
lbf:
# LambdaBuffers Haskell Codegen
lbg-haskell:
{
  # Nixpkgs
  pkgs
, # Source that are passed to `lbf` as the `--import-path` flag and used to find `files`.
  # Examples: src = [ ./api ]
  src
, # Additional sources that are passed to `lbf` as the `--import-path` flag.
  # Examples: imports = [ lbf-prelude ]
  imports ? [ ]
, # .lbf files in `src` to compile and codegen.
  # Examples: files = [ "Foo.lbf" "Foo/Bar.lbf" ]
  files
, # Dependencies to include in the Cabal's `build-depends` stanza.
  # examples: dependencies = [ "lbf-prelude" "lbr-prelude" ]
  dependencies ? [ ]
, # Name of the package and also the name of the Cabal package.
  # Examples: name = "lbf-myproject"
  name
, # Version of the package and also the version of the Cabal package.
  # Examples: version = "0.1.0.0"
  version ? "0.1.0.0"
}:
let
  cabalTemplate = pkgs.writeTextFile {
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
          build-depends: <DEPS>
    '';
  };
in
pkgs.stdenv.mkDerivation {
  inherit src version;
  pname = name;
  outputs = [ "out" "build" ];
  buildInputs = [
    pkgs.cabal-install
    lbf
    pkgs.jq
  ];
  buildPhase = ''
    mkdir autogen
    mkdir .work
    ls
    lbf build ${builtins.concatStringsSep " " (builtins.map (src: "--import-path ${src}") ([src] ++ imports))} \
        --work-dir .work \
        --gen ${lbg-haskell}/bin/lbg-haskell \
        --gen-dir autogen \
        ${builtins.concatStringsSep " " files}

    EXPOSED_MODULES=$(find autogen -name "*.hs" | while read f; do grep -Eo 'module\s+\S+\s+' $f | head -n 1 | sed -r 's/module\s+//' | sed -r 's/\s+//'; done | tr '\n' ' ')
    echo "Found generated modules $EXPOSED_MODULES"
    DEPS=$(echo ${builtins.concatStringsSep " " dependencies} $(cat autogen/build.json | jq -r ".[]") | tr ' ' ',' | sed 's/$//')

    cat ${cabalTemplate} \
        | sed -r "s/<EXPOSED_MODULES>/$EXPOSED_MODULES/" \
        | sed -r "s/<DEPS>/$DEPS/" > ${name}.cabal
    cat ${name}.cabal
    cat autogen/build.json
  '';

  installPhase = ''
    mkdir -p $out/autogen;
    cp -r autogen $out
    cp ${name}.cabal $out/${name}.cabal;
    mv autogen/build.json $build;
  '';
}
