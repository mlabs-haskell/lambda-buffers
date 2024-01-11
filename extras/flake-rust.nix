pkgs:

{ crane
, src
, crateName
, rustVersion ? "latest"
, nativeBuildInputs ? [ ]
, extraSources ? [ ]
, extraSourcesDir ? ".extras"
, data ? [ ]
, dataDir ? "data"
, devShellHook ? ""
, devShellTools ? [ ]
, testTools ? [ ]
}:
let
  rustWithTools = pkgs.rust-bin.stable.${rustVersion}.default.override {
    extensions = [ "rustfmt" "rust-analyzer" "clippy" "rust-src" ];
  };
  craneLib = crane.lib.${pkgs.system}.overrideToolchain rustWithTools;

  cleanSrc = craneLib.cleanCargoSource (craneLib.path src);

  # Library source code with extra dependencies copied
  buildEnv =
    pkgs.stdenv.mkDerivation
      {
        src = cleanSrc;
        name = "${crateName}-build-env";
        unpackPhase = ''
          mkdir $out
          cp -r $src/* $out
          cd $out
          ${copyExtraSources}
          ${copyData}
        '';
      };

  # Library source code, intended to be in extraSourcesDir
  # Dependencies of this crate are not copied, to the extra sources directory
  # but they are referenced from the parent directory (parent crate's extra sources).
  vendoredSrc =
    pkgs.stdenv.mkDerivation
      {
        src = cleanSrc;
        name = "${crateName}-vendored-src";
        unpackPhase = ''
          mkdir $out
          cp -r $src/* $out
          cd $out
          sed -i 's/${pkgs.lib.escapeRegex extraSourcesDir}/../g' Cargo.toml
        '';
      };

  commonArgs = {
    inherit nativeBuildInputs;
    src = buildEnv;
    pname = crateName;
    strictDeps = true;
  };
  cargoArtifacts = craneLib.buildDepsOnly commonArgs;

  # Extra sources
  extra-sources = pkgs.linkFarm "extra-sources" extraSources;

  hasExtraSources = builtins.length extraSources > 0;
  linkExtraSources = pkgs.lib.optionalString hasExtraSources ''
    echo "Linking extra sources"
    if [ -e ./${extraSourcesDir} ]; then rm ./${extraSourcesDir}; fi
    ln -s ${extra-sources} ./${extraSourcesDir}
  '';
  copyExtraSources = pkgs.lib.optionalString hasExtraSources ''
    echo "Copying extra sources"
    cp -Lr ${extra-sources} ./${extraSourcesDir}
  '';

  # Data
  data-drv = pkgs.linkFarm "data" data;
  hasData = builtins.length data > 0;
  linkData = pkgs.lib.optionalString hasData ''
    echo "Linking data"
    if [ -e ./${dataDir} ]; then rm ./${dataDir}; fi
    ln -s ${data-drv} ./${dataDir}
  '';
  copyData = pkgs.lib.optionalString hasData ''
    echo "Copying data"
    cp -Lr ${data-drv} ./${dataDir}
  '';
in
{
  devShells."dev-${crateName}-rust" = craneLib.devShell {
    buildInputs = nativeBuildInputs;
    packages = devShellTools;
    shellHook = ''
      ${linkExtraSources}
      ${linkData}
      ${devShellHook}
    '';
  };

  packages = {
    "${crateName}-rust" = craneLib.buildPackage (commonArgs // {
      inherit cargoArtifacts;
      doCheck = false;
      doInstallCargoArtifacts = true;
    });

    "${crateName}-rust-src" = vendoredSrc;

    "${crateName}-rust-build-env" = buildEnv;
  };

  checks = {
    "${crateName}-rust-test" = craneLib.cargoNextest (commonArgs // {
      inherit cargoArtifacts;
      nativeBuildInputs = testTools;
    });

    "${crateName}-rust-clippy" = craneLib.cargoClippy (commonArgs // {
      inherit cargoArtifacts;
    });
  };
}
