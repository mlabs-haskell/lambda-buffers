# Build .lbf schemas and generate Haskell's PlutusTX library.
pkgs: lbf: lbg-plutustx: lbfPlutusTxOpts:
import ./lbf-haskell.nix pkgs lbf lbg-plutustx lbfPlutusTxOpts
