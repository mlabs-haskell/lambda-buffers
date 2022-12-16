{ pkgs, preCommitTools, shellHook }:
pkgs.mkShell {
  name = "docs-env";

  packages = [ preCommitTools.markdownlint-cli preCommitTools.typos ];

  inherit shellHook;
}
