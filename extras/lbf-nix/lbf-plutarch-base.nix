# Build .lbf schemas and generate Haskell's Plutarch library.
pkgs: lbf: lbg-plutarch: lbfPlutarchOpts:
import ./lbf-haskell.nix pkgs lbf lbg-plutarch lbfPlutarchOpts
