# plutus-ledger-api from crates.io, with its `lbr-prelude` dependency pointed at
# this repo's runtime so generated lbf-plutus code sees a single lbr-prelude.
# Used via `extraSources` (named like a rustFlake `-rust-src` package).
_: {
  perSystem =
    { pkgs, ... }:
    {
      packages.plutus-ledger-api-rust-src = pkgs.runCommand "plutus-ledger-api-3" {
        src = pkgs.fetchurl {
          name = "plutus-ledger-api-3.1.0.tar.gz";
          url = "https://crates.io/api/v1/crates/plutus-ledger-api/3.1.0/download";
          sha256 = "2b1e082eae586833f73a2a43336b45403c0700b78e3c0881c90fb827b8af8a65";
        };
      } ''
        mkdir $out
        tar xzf $src -C $out --strip-components=1
        sed -i '/^\[dependencies.lbr-prelude\]$/,/^$/s|^version = .*|path = "../lbr-prelude-v0"|' $out/Cargo.toml
        grep -q 'path = "../lbr-prelude-v0"' $out/Cargo.toml
      '';
    };
}
