{ pkgs, lbfPurescript, lbf-prelude-purs, lbr-prelude-purs, commonTools, shellHook }:
{
  inherit pkgs;
  src = ./.;
  projectName = "lbt-prelude";
  strictComp = true;
  packageJson = ./package.json;
  packageLock = ./package-lock.json;

  extraSources = [
    (lbfPurescript {
      inherit pkgs;
      name = "lbf-golden";
      src = ./../api;
      files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
      imports = [ ../../../libs/lbf-prelude ];
    })
    lbf-prelude-purs
    lbr-prelude-purs
  ];
  goldenData = import ../../../extras/haskell-data.nix {
    inherit pkgs;
    srcs = [ ../. ];
    cabalDataPatterns = [ "**/*.lbf" "**/*.json" ];
    cabalPackageName = "lbt-prelude-golden-data-hs";
  };

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
