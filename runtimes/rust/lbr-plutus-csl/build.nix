{ inputs, ... }: {
  perSystem = { self', pkgs, system, ... }:
    let
      crateName = "lbr-plutus-csl";
      rustWithTools = pkgs.rust-bin.stable.latest.default.override {
        extensions = [ "rustfmt" "rust-analyzer" "clippy" ];
      };
      craneLib = inputs.crane.lib.${system}.overrideToolchain rustWithTools;
      src = craneLib.cleanCargoSource (craneLib.path ./.);
      commonArgs = {
        inherit src;
        strictDeps = true;
      };
      cargoArtifacts = craneLib.buildDepsOnly commonArgs;
    in
    {
      devShells."dev-${crateName}-rust" = craneLib.devShell {
        checks = self'.checks;
      };

      packages."${crateName}-rust" = craneLib.buildPackage (commonArgs // {
        inherit cargoArtifacts;
        doTest = false;
      });

      checks."${crateName}-rust-test" = craneLib.cargoNextest (commonArgs // {
        inherit cargoArtifacts;
      });

      checks."${crateName}-rust-clippy" = craneLib.cargoClippy (commonArgs // {
        inherit cargoArtifacts;
      });
    };

}
