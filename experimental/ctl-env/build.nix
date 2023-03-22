{ system, nixpkgs, ctl, lbf, lbc, lbg }:
let
  nixpkgsFor = system: import nixpkgs {
    inherit system;
    overlays = [
      ctl.overlays.purescript
      ctl.overlays.runtime
      ctl.overlays.spago
    ];
  };
  pkgs = nixpkgsFor system;
in
(pkgs.purescriptProject {
  inherit pkgs;
  projectName = "lambda-buffers-ctl-env";
  strictComp = false; # TODO: this should be eventually removed
  src = ./.;
  shell = {
    packageLockOnly = true;
    packages = with pkgs; [
      bashInteractive
      fd
      nodePackages.eslint
      nodePackages.prettier
      lbf
      lbc
      lbg
    ];
    shellHook =
      ''
        export LC_CTYPE=C.UTF-8
        export LC_ALL=C.UTF-8
        export LANG=C.UTF-8
      '';
  };
}).devShell
