pkgs:

{ crane, src, crateName, extraSources ? [ ], extraSourcesDir ? ".extras", data ? [ ], dataDir ? "data", devShellHook ? "", devShellTools ? [ ], testTools ? [ ] }:
let
  rustWithTools = pkgs.rust-bin.stable.latest.default.override {
    extensions = [ "rustfmt" "rust-analyzer" "clippy" "rust-src" ];
  };
  craneLib = crane.lib.${pkgs.system}.overrideToolchain rustWithTools;

  # Library source code with extra dependencies attached
  fullSrc = pkgs.stdenv.mkDerivation {
    src = craneLib.cleanCargoSource (craneLib.path src);
    name = "lbf-rust-build-env";
    unpackPhase = ''
      mkdir $out
      cp -r $src/* $out
      cd $out
      ${copyExtraSources}
      ${copyData}
    '';
  };
  commonArgs = {
    src = fullSrc;
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
    packages = devShellTools;
    shellHook = ''
      ${linkExtraSources}
      ${linkData}
      ${devShellHook}
    '';
  };

  packages."${crateName}-rust" = craneLib.buildPackage (commonArgs // {
    inherit cargoArtifacts;
    doCheck = false;
    doInstallCargoArtifacts = true;
  });

  checks."${crateName}-rust-test" = craneLib.cargoNextest (commonArgs // {
    inherit cargoArtifacts;
    nativeBuildInputs = testTools;
  });

  checks."${crateName}-rust-clippy" = craneLib.cargoClippy (commonArgs // {
    inherit cargoArtifacts;
  });
}
