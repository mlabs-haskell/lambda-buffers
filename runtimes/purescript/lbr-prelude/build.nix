{ pkgs, commonTools, shellHook }:
pkgs.purescriptProject {
  inherit pkgs;
  src = ./.;
  projectName = "lbr-prelude-purescript";
  strictComp = false; # TODO: this should be eventually removed
  packageJson = ./package.json;
  packageLock = ./package-lock.json;
  shell = {
    withRuntime = false;
    packageLockOnly = true;
    packages = builtins.attrValues commonTools ++ [
      pkgs.bashInteractive
      pkgs.fd
    ];
    shellHook = ''
      export LC_CTYPE=C.UTF-8
      export LC_ALL=C.UTF-8
      export LANG=C.UTF-8
      ${shellHook}
    '';
  };
}
