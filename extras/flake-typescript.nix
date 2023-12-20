pkgs:
{ name
, src
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

  npmDependencies ? [ ]
, # The script to build the project i.e., `npm run ${npmBuildScript}` is
  # executed.
  npmBuildScript ? "build"
, nodejs ? pkgs.nodejs-18_x
, # `devShellHook` is the shell commands to run _before_  entering the shell
  # (see the variable `shell`)
  devShellHook ? ""
, # `devShellTools` are extra packages one may use in the dev shell
  devShellTools ? [ ]
, # `testTools` are extra derivations to append to the `buildInputs` for
  # the tests (see the variable `test`)
  testTools ? [ ]
, ...
}:
let
  npmLocalDependencies =
    pkgs.callPackage (import ./typescript/npm-local-dependencies.nix) { } { inherit name npmDependencies; };

  projectNode2nix = pkgs.stdenv.mkDerivation {
    name = "${name}-node2nix";
    inherit src;
    buildInputs = [ pkgs.node2nix nodejs ];
    configurePhase =
      ''
        ${npmLocalDependencies.npmLocalDependenciesLinkCommand}
      '';

    buildPhase =
      ''
        NIX_NODE_ENV_FILE=./node-env.nix
        NIX_NODE_PACKAGES_FILE=./node-packages.nix
        NIX_COMPOSITION_FILE=./default.nix

        # Make sure these files are generated with something like
        # ```
        # npm install --package-lock-only --lockfile-version 2
        # ```
        node2nix --input ./package.json --lock ./package-lock.json --development --node-env "$NIX_NODE_ENV_FILE" --output "$NIX_NODE_PACKAGES_FILE" --composition "$NIX_COMPOSITION_FILE"
      '';

    installPhase =
      ''
        mkdir -p $out
        cp -r ./. $out
      '';
  };

  projectNode2nixAttrs = import projectNode2nix { inherit nodejs pkgs; inherit (pkgs) system; };

  # Important note:
  # Inspection of the code suggests that the node_modules are put in 
  # ```
  # $out/lib/node_modules/${projectNode2nixAttrs.args.packageName}/node_modules
  # ```
  npmPackage = projectNode2nixAttrs.package.override
    {
      # Ensures that the node_modules has the extra linked dependencies when
      # building it.
      preRebuild =
        ''
          ${npmLocalDependencies.npmLocalDependenciesLinkCommand}
        '';

      # TODO(jaredponn): Wow this is horrible. `npm install` is broken for
      # local dependencies on the filesystem. I think something the following
      # is problematic [this may not be an issue for later versions /
      # lockfiles? needs more investigations]:
      # - Suppose A is a tarball and depends on tarball B
      # - Assume that we have a "sensible" `package.json` and
      # `package-lock.json` with `./foo/A` and `./foo/B` installed
      # - If we do _not_ have `node_modules`, and try to `npm install`, `npm
      # install` will get confused and try to look in `node_modules/A/foo/B`
      # which obviously doesn't exist so it errors.
      # Apparently removing `package-lock.json` fixes this, so it can rebuild
      # it from scratch I guess?
      postRebuild =
        ''
          rm package-lock.json
        '';
    };

  # Build the project (runs `npm run build`)
  # TODO(jaredponn): perhaps we should do something else instead of just
  # dumping everything to the nix store..
  project = pkgs.stdenv.mkDerivation {
    name = "${name}-typescript";
    inherit src;
    buildInputs = [ nodejs ];

    configurePhase =
      ''
        runHook preConfigure
            
        ln -sf ${npmPackage}/lib/node_modules/${projectNode2nixAttrs.args.packageName}/node_modules node_modules
            
        runHook postConfigure
      '';

    buildPhase =
      ''
        export HOME=$(mktemp -d)
        export NPM_CONFIG_OFFLINE=true
        export NPM_CONFIG_LOGLEVEL=verbose

        npm run ${npmBuildScript}
      '';

    installPhase =
      ''
        mkdir -p $out
        cp -r ./. $out
      '';
  };

  shell = pkgs.mkShell {
    packages = [ nodejs ] ++ devShellTools;

    shellHook =
      ''
        ${devShellHook}

        linkNpmDependencies( ) {
            ${npmLocalDependencies.npmLocalDependenciesLinkCommand}
        }

        echo 'Executing `linkNpmDependencies` to link dependencies from nix...' 
        linkNpmDependencies
      '';

  };

  # Creates a tarball of `project` using `npm pack` and puts it in the nix
  # store.
  npmPack = project.overrideAttrs (_self: _super:
    {
      name = "${name}-tarball";
      installPhase =
        ''
          mkdir -p $out/tarballs
          npm pack --pack-destination $out/tarballs
        '';
    });


  # Run tests with `npm test`.
  test = project.overrideAttrs (_self: super:
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
    "${name}-typescript-nix-npm-folder-dependencies" = npmLocalDependencies.npmLocalDependenciesDerivation;
    "${name}-typescript-node2nix" = projectNode2nix;
  };

  checks = {
    "${name}-typescript-test" = test;
  };
}
