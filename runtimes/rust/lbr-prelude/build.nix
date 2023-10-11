{ inputs, ... }: {
  imports = [ inputs.nci.flakeModule ];
  perSystem = { pkgs, system, inputs', config, ... }:
    let crateName = "lbr-prelude";
    in {
      nci.projects.${crateName}.path = ./.;
      nci.crates.${crateName} = { };

      devShells."dev-${crateName}-rust" = config.nci.outputs.${crateName}.devShell.overrideAttrs (old: {
        packages = (old.packages or [ ]) ++ [
          pkgs.rust-analyzer
          pkgs.clippy
        ];
      });
      packages."${crateName}-rust" = config.nci.outputs.${crateName}.packages.release;
      checks."${crateName}-rust-test" = config.nci.outputs.${crateName}.check;
    };

}
