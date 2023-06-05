{ pkgs, src, lbfFile, importPaths, lbf, lbg-haskell, cabalPackageName, deps ? [ ], cabalPackageVersion ? "0.1.0.0" }:
let
  importPaths' = builtins.concatStringsSep " " (builtins.map (imp: "--import-path ${imp}") importPaths);
  providedDeps = builtins.concatStringsSep " " (builtins.map (dep: dep.name) deps);
  cabalTemplate = pkgs.writeTextFile {
    name = "lambda-buffers-cabal-template";
    text = ''
      cabal-version:      3.0
      name:               ${cabalPackageName}
      version:            ${cabalPackageVersion}
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
  inherit src;
  name = cabalPackageName;
  outputs = [ "out" "build" ];
  buildInputs = [
    pkgs.cabal-install
    lbf
    pkgs.jq
  ];
  buildPhase = ''
    mkdir autogen
    mkdir .work
    lbf build ${importPaths'} \
        --file ${src}/${lbfFile} \
        --work-dir .work \
        --gen ${lbg-haskell}/bin/lbg-haskell \
        --gen-dir autogen

    EXPOSED_MODULES=$(find autogen -name "*.hs" | while read f; do grep -Eo 'module\s+\S+\s+' $f | head -n 1 | sed -r 's/module\s+//' | sed -r 's/\s+//'; done | tr '\n' ' ')
    echo "Found generated modules $EXPOSED_MODULES"
    DEPS=$(echo ${providedDeps} $(cat autogen/build.json | jq -r ".[]") | tr ' ' ',' | sed 's/.$//')

    cat ${cabalTemplate} \
        | sed -r "s/<EXPOSED_MODULES>/$EXPOSED_MODULES/" \
        | sed -r "s/<DEPS>/$DEPS/" > ${cabalPackageName}.cabal
  '';

  installPhase = ''
    mkdir -p $out/autogen;
    cp -r autogen $out
    cp ${cabalPackageName}.cabal $out/${cabalPackageName}.cabal;
    mv autogen/build.json $build;
  '';
}
