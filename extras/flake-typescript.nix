pkgs:
{ name
, src
, npmDepsHash ? pkgs.lib.fakeHash
, # `dependencies` is of type
  # ```
  # [ nix derivation for a tarball from `npm pack` ]
  # ```
  # for the the extra dependencies (not included in the `package.json`) for
  # `node` to execute. This will _not_ install the "transitive" dependencies.
  # 
  # Loosely, this will (in the order given) copy each tarball to a local
  # directory, call `npm cache` on the tarball, and finally call `npm install`.
  #
  # For example, if one wanted to include `typescript` as a dependency, then
  # one could have
  #
  # ```
  # let pkgs = import <nixpkgs> {}
  #     dependencies = [
  #           (pkgs.fetchurl {
  #             url = "https://registry.npmjs.org/typescript/-/typescript-5.2.2.tgz";
  #             sha512 = "mI4WrpHsbCIcwT9cF4FZvr80QUeKvsUsUvKDoR+X/7XHQH98xYD8YHZg7ANtz2GtZt/CBq2QJ0thkGJMHfqc1w==";
  #           })
  #    ];
  # in ...
  # ```

  npmDependencies ?  [ ]

, 
    # The script to build the project i.e., `npm run ${npmBuildScript}` is
    # executed.
    npmBuildScript ? "build"
, nodejs ? pkgs.nodejs-18_x
, # `devShellHook` is the shell commands to run _before_  entering the shell
  # (see the variable `shell`)
  devShellHook ? ""
, # `devShellTools` are extra derivations to append to the `buildInputs` for
  # the shell (see the variable `shell`)
  devShellTools ? [ ]
, # `testTools` are extra derivations to append to the `buildInputs` for
  # the tests (see the variable `test`)
  testTools ? [ ]
, ...
}:
let
  npmLocalDependencies = 
    pkgs.callPackage (import ./typescript/npm-local-dependencies.nix) {} { inherit name npmDependencies; };

  fetchedPackageLock = 
    pkgs.callPackage (import ./typescript/fetch-package-lock.nix) {} 
        { inherit src ; };

  npmPackage = pkgs.stdenv.mkDerivation {
    inherit name src npmDepsHash;
    buildInputs = [ nodejs ];

    configurePhase = 
        ''
            runHook preConfigure

            export HOME=$(mktemp -d)
            export NPM_CONFIG_OFFLINE=true
            export NPM_CONFIG_LOGLEVEL=verbose

            ${builtins.concatStringsSep "\n"
                (builtins.map
                    (dep:
                        if dep.packageResolvedFetched == null then "" else
                        ''
                            echo 'Adding `${dep.packageResolvedFetched}` to npm's cache'
                            npm cache add "${dep.packageResolvedFetched}"
                        ''
                    )
                    fetchedPackageLock
                )
            }

            ${npmLocalDependencies.npmLocalDependenciesLinkCommand}

            npm install
            
            runHook postConfigure
        '';

    buildPhase = 
        ''
            npm run ${npmBuildScript}
        '';
  };

  # Build the project (runs `npm run build`), and puts the entire output in the
  # nix store
  project = npmPackage;

  shell =  npmPackage.overrideAttrs (_self: super:
    {
        shellHook = 
            ''
                linkNpmDependencies( ) {
                    ${npmLocalDependencies.npmLocalDependenciesLinkCommand}
                }

                echo 'Executing `linkNpmDependencies` to link dependencies from nix...' 
                linkNpmDependencies
            '';

        buildInputs = super.buildInputs ++ devShellTools;
    });

  # Creates a tarball of `project` using `npm pack` and puts it in the nix
  # store.
  npmPack =  npmPackage.overrideAttrs (_self: _super:
    {
        name = "${name}.tgz";
        installPhase =
          ''
            tgzFile=$(npm --log-level=verbose pack | tail -n 1)
            mv $tgzFile $out
          '';
    });


  # Run tests with `npm test`.
  test = npmPackage.overrideAttrs (_self: super:
    {
      # Append the build command at the end.
      postBuild =
        ''
            npm --log-level=verbose test
        '';
      installPhase =
        ''
            touch $out
        '';
      buildInputs = super.buildInputs ++ testTools;
    });

in
{
  devShells = {
    "${name}-typescript" = shell;
  };

  packages = {
    "${name}-typescript" = project;
    "${name}-typescript-tgz" = npmPack;
    "${name}-nix-npm-folder-dependencies-typescript" = npmLocalDependencies.npmLocalDependenciesDerivation;
  };

  checks = {
    "${name}-typescript-test" = test;
  };
}
