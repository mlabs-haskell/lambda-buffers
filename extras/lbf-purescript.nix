# LambdaBuffers Frontend
lbf:
# LambdaBuffers Purescript Codegen
lbg-purescript:
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
, # TODO(bladyjoker): Dependencies to include in the `build` output
  # examples: dependencies = [ "lbf-prelude" "lbr-prelude" ]
  dependencies ? [ ]
, # Name of the package and also the name of the Cabal package.
  # Examples: name = "lbf-myproject"
  name
, # Version of the package and also the version of the Cabal package.
  # Examples: version = "0.1.0.0"
  version ? "0.1.0.0"
}:
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
    lbf build ${builtins.concatStringsSep " " (builtins.map (src: "--import-path ${src}") ([src] ++ imports))} \
        --work-dir .work \
        --gen ${lbg-purescript}/bin/lbg-purescript \
        --gen-dir autogen \
        ${builtins.concatStringsSep " " files}

    cat autogen/build.json
  '';

  installPhase = ''
    mkdir -p $out/src;
    cp -r autogen/* $out/src
    mv autogen/build.json $build;
  '';
}
