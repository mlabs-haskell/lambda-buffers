# Makes a Haskell Flake using haskell.nix with a simplified interface.
pkgs:
let
  haskellNixOpts =
    { src
    , name
    , dependencies ? [ ]
    , devShellTools ? [ ]
    , devShellHook
    , devShellAdditionalPackages ? _: [ ]
    , index-state
    , compiler-nix-name
    , modules ? [ ]
    }: {
      inherit src name dependencies devShellTools devShellHook index-state compiler-nix-name modules devShellAdditionalPackages;
    };

  hsNixProj = opts: with (haskellNixOpts opts);
    let
      proj = { lib, ... }: {
        inherit src name index-state compiler-nix-name;
        extraHackage = dependencies;
        modules = [
          (_: {
            packages = {
              allComponent.doHoogle = true;
              allComponent.doHaddock = true;

              # Enable strict compilation
              "${name}".configureFlags = [ "-f-dev" ];
            };
          })
        ];

        shell = {
          withHoogle = true;
          exactDeps = true;
          nativeBuildInputs = devShellTools;
          additional = devShellAdditionalPackages;

          tools = {
            cabal = { };
            haskell-language-server = { };
          };

          shellHook = lib.mkForce devShellHook;
        };

      };
    in
    proj;

  hsNixFlake = opts: with (haskellNixOpts opts);
    (pkgs.haskell-nix.cabalProject' ([
      ((import ./haskell.nix/extra-hackage.nix) compiler-nix-name)
      (hsNixProj opts)
    ]
    ++ modules
    )
    ).flake { };

in
hsNixFlake
