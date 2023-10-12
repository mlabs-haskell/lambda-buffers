{ inputs, ... }: {
  imports = [ inputs.nci.flakeModule ];
  perSystem = { pkgs, system, inputs', config, ... }:
    let crateName = "lbr-prelude";
    in {
      nci.projects.${crateName}.path = ./.;
      nci.crates.${crateName} = { };
      nci.toolchainConfig = {
        channel = "stable";
        components = [ "rust-analyzer" "rust-src" "clippy" "rustfmt" ];
      };

      devShells."dev-${crateName}-rust" = config.nci.outputs.${crateName}.devShell;
      packages."${crateName}-rust" = config.nci.outputs.${crateName}.packages.release;
      checks."${crateName}-rust-test" = config.nci.outputs.${crateName}.check;
    };

}
