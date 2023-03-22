{ pkgs
, haskell-nix
, mlabs-tooling
, compiler-nix-name
, index-state
, lbf
, lbc
, lbg
, lbf-base
}:
((haskell-nix.cabalProject' [
  mlabs-tooling.lib.mkHackageMod
  mlabs-tooling.lib.moduleMod
  ({ lib, ... }: {
    src = ./.;
    name = "lambda-buffers-plutustx-env";
    inherit compiler-nix-name index-state;
    shell = {
      nativeBuildInputs = [ lbc lbf lbg pkgs.bashInteractive ];
      shellHook = lib.mkForce ''
        rm -f lbf-base
        ln -s ${lbf-base} lbf-base
      '';
    };
  })
]).flake { }).devShell
