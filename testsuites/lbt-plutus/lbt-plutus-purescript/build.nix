{ pkgs, lbfPurescript, lbf-prelude-purs, lbf-plutus-purs, lbr-prelude-purs, lbr-plutus-purs, commonTools, shellHook }:
{
  inherit pkgs;
  src = ./.;
  projectName = "lbt-plutus";
  strictComp = true;
  packageJson = ./package.json;
  packageLock = ./package-lock.json;

  extraSources = [
    (lbfPurescript {
      inherit pkgs;
      name = "lbf-golden";
      src = ./../api;
      files = [ "Foo.lbf" "Foo/Bar.lbf" "Days.lbf" ];
      imports = [ ../../../libs/lbf-prelude ../../../libs/lbf-plutus ];
    })
    lbf-prelude-purs
    lbf-plutus-purs
    lbr-prelude-purs
    lbr-plutus-purs
  ];
  data = [
    {
      name = "lbt-plutus-golden-data";
      path = ../golden;
    }
  ];

  shell = {
    withRuntime = false;
    packageLockOnly = true;
    packages = builtins.attrValues commonTools ++ [
      pkgs.nodejs_16
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
