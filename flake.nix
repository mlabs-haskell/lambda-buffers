{
  description = "Lambda Buffers";
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    protobufs-nix.url = "github:mlabs-haskell/protobufs.nix";
  };

  outputs = { self, nixpkgs, haskell-nix, flake-utils, pre-commit-hooks, protobufs-nix }: flake-utils.lib.eachSystem [ "x86_64-linux" ]
    (system:
      let
        inherit self haskell-nix;

        pkgs = import nixpkgs {
          inherit system;
        };

        # pre-commit-hooks.nix
        fourmolu = pkgs.haskell.packages.ghc924.fourmolu;

        pre-commit-check = pre-commit-hooks.lib.${system}.run (import ./pre-commit-check.nix {
          inherit pkgs fourmolu;
        });

        preCommitTools = pre-commit-hooks.outputs.packages.${system};

        pre-commit-devShell = pkgs.mkShell {
          name = "pre-commit-env";
          inherit (pre-commit-check) shellHook;
        };

        # Experimental env
        experimentalDevShell = import ./experimental/build.nix {
          inherit pkgs preCommitTools;
          inherit (pre-commit-check) shellHook;
        };

        # Docs env
        docsDevShell = import ./docs/build.nix {
          inherit pkgs preCommitTools;
          inherit (pre-commit-check) shellHook;
        };

        # Protos env
        pbnix-lib = protobufs-nix.lib.${system};

        protosBuild = import ./lambda-buffers-proto/build.nix {
          inherit pkgs pbnix-lib;
          inherit (pre-commit-check) shellHook;
        };

        # Utilities
        # INFO: Will need this; renameAttrs = rnFn: pkgs.lib.attrsets.mapAttrs' (n: value: { name = rnFn n; inherit value; });
      in
      rec {
        # Useful for nix repl
        inherit pkgs;

        # Standard flake attributes
        packages = { inherit (protosBuild) compilerHsPb; };

        devShells = rec {
          dev-pre-commit = pre-commit-devShell;
          dev-experimental = experimentalDevShell;
          dev-docs = docsDevShell;
          dev-protos = protosBuild.protosDevShell;
          default = pre-commit-devShell;
        };

        # nix flake check --impure --keep-going --allow-import-from-derivation
        checks = { inherit pre-commit-check; } // devShells // packages;

      }
    ) // {
    # Instruction for the Hercules CI to build on x86_64-linux only, to avoid errors about systems without agents.
    herculesCI.ciSystems = [ "x86_64-linux" ];
  };
}
