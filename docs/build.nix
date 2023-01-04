{ pkgs, commonTools, shellHook }:
pkgs.mkShell {
  name = "docs-env";

  packages = [ commonTools.markdownlint-cli commonTools.typos ];

  inherit shellHook;
}
