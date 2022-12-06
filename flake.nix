{
  description = "Lambda Buffers";
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = { self, nixpkgs, haskell-nix, flake-utils, pre-commit-hooks }: flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      inherit self haskell-nix;

      pkgs = import nixpkgs {
        inherit system;
      };

      # pre-commit-hooks.nix
      fourmolu = pkgs.haskell.packages.ghc924.fourmolu;

      pre-commit-check = pre-commit-hooks.lib.${system}.run (import ./pre-commit-check.nix {
        inherit fourmolu;
      });

      pre-commit-devShell = pkgs.mkShell {
        inherit (pre-commit-check) shellHook;
      };

      # Utilities
      # INFO: Will need this; renameAttrs = rnFn: pkgs.lib.attrsets.mapAttrs' (n: value: { name = rnFn n; inherit value; });
    in
    rec {
      # Useful for nix repl
      inherit pkgs;

      # Instruction for the Hercules CI to build on x86_64-linux only, to avoid errors about systems without agents.
      herculesCI.ciSystems = [ "x86_64-linux" ];

      # Standard flake attributes
      packages = { };

      devShells = rec {
        dev-pre-commit = pre-commit-devShell;
        default = pre-commit-devShell;
      };

      # nix flake check --impure --keep-going --allow-import-from-derivation
      checks = { inherit pre-commit-check; } // devShells // packages;

    }
  );
}
