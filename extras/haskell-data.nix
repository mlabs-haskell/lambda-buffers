# Makes a Cabal package with just a 'data' directory with proper 'data-filers' stanza
pkgs: { srcs, cabalDataPatterns, cabalPackageName, cabalPackageVersion ? "0.1.0.0" }:
let
  cabalTemplate = pkgs.writeTextFile {
    name = "haskell-data.nix-cabal-template";
    text = ''
      cabal-version:      3.0
      name:               ${cabalPackageName}
      version:            ${cabalPackageVersion}
      synopsis:           A Cabal project that contains data files
      build-type:         Simple
      data-files: ${builtins.concatStringsSep ", " (builtins.map (d: "data/${d}") cabalDataPatterns)}

      library
          default-language: Haskell2010
          build-depends: base >=4.16
          exposed-modules: Paths_${builtins.replaceStrings ["-"] ["_"] cabalPackageName}
    '';
  };
in
pkgs.stdenv.mkDerivation {
  inherit srcs;
  name = cabalPackageName;
  buildInputs = [
    pkgs.cabal-install
  ];
  sourceRoot = ".";
  buildPhase = ''
    mkdir data;
    cp -r -t data ${builtins.concatStringsSep " " (builtins.map (src: "${src}/*") srcs)};
    cat ${cabalTemplate} > ${cabalPackageName}.cabal;
  '';

  installPhase = ''
    mkdir $out;
    cp -r data $out/;
    mv ${cabalPackageName}.cabal $out/;
  '';
}
