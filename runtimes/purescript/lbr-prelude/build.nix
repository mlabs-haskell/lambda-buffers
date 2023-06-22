{ pkgs, commonTools, shellHook }:
{
  inherit pkgs;
  src = ./.;
  projectName = "lbr-prelude";
  strictComp = true;
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
