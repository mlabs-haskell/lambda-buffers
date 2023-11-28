# Makes a Haskell Flake using haskell.nix with a simplified interface with support for Plutus development.
cardano-haskell-packages: pkgs: opts:
let
  plutusMod = (import ./haskell.nix/plutus.nix) opts.compiler-nix-name cardano-haskell-packages;
  opts' =
    if "modules" ? opts then opts // {
      modules = opts.modules ++ [ plutusMod ];
    }
    else opts // { modules = [ plutusMod ]; };
in
(import ./flake-haskell.nix pkgs) opts'
