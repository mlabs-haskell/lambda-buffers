# LambdaBuffers Frontend
lbf:
# LambdaBuffers Purescript Codegen
lbg-purescript:
{
  # Nixpkgs
  pkgs
, # Source that is passed to `lbf` as the `--import-path` flag and used to find `files`.
  # Examples: src = ./api
  src
, # Additional sources that are passed to `lbf` as the `--import-path` flag.
  # Examples: imports = [ lbf-prelude ]
  imports ? [ ]
, # .lbf files in `src` to compile and codegen.
  # Examples: files = [ "Foo.lbf" "Foo/Bar.lbf" ]
  files
  # Classes for which to generate implementations for.
, classes ? [ "Prelude.Eq" "Prelude.Json" ]
, # TODO(bladyjoker): Dependencies to include in the `build` output
  # examples: dependencies = [ "lbf-prelude" "lbr-prelude" ]
  dependencies ? [ ]
, # Package name.
  # Examples: name = "lbf-myproject"
  name
, # Package version.
  # Examples: version = "0.1.0.0"
  version ? "0.1.0.0"
}:
let
  utils = import ./utils.nix pkgs;
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
    lbf build ${utils.mkFlags "import-path" imports} ${utils.mkFlags "gen-class" classes} \
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
