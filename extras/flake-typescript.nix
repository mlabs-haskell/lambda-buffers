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

  dependencies ? [ ]

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
  packageLockDependencies = import ./typescript/fetch-package-lock.nix pkgs { inherit src; };

  # Derivation for
  # 1. Caching (via npm cache) all dependencies already existing in the package-lock.json
  # 2. Caching the extra dependencies provided by nix
  # 3. Installing the extra dependencies provided by  nix to the package-lock.json
  # 4. npm install all of the above
  npmProject =
    pkgs.runCommand (name + "-npm-project") { buildInputs = [ nodejs pkgs.jq ]; }
      ''
        # For some reason, npm likes to call `mkdir $HOME` (which is not
        # writeable when building derivations on nix), so we change `HOME` to a
        # writable directory
        export HOME=$TMPDIR/home
        echo "Set HOME to $HOME"

        # Create a directory for npm's cache
        mkdir -p $TMPDIR/cache
        npm config set cache $TMPDIR/cache

        # Force NPM to remain offline (if it hits the network, it'll fail
        # anyways because of nix's isolated build environment
        npm config set offline=true

        mkdir -p "$out"

        cd "$out"

        cp -r ${src}/* .

        #########################################################
        # Verify that `package.json` and `package-lock.json` exist
        #########################################################
        if ! test -f package.json
        then { echo 'No `package.json` provided'; exit 1; }
        fi

        if ! test -f package-lock.json
        then { echo 'No `package-lock.json` provided. Running `npm install --package-lock-only` may fix this'; exit 1; }
        fi

        #########################################################
        # Create tarballs of all dependencies already in the lock file
        #########################################################

        # We write the list of `packageLockDependencies` as
        # `<dependency1>`, `<dependency2>`, ..., `<dependencyN>`
        # this will create:
        # ```
        # echo "Adding <dependency1> to npm's cache"
        # npm cache add --loglevel-verbose <dependency1>
        #
        # echo "Adding <dependency2> to npm's cache"
        # npm cache add --loglevel-verbose <dependency2>
        #
        # ...
        #
        # echo "Adding <dependencyN> to npm's cache"
        # npm cache add --loglevel-verbose <dependencyN>
        # ```
        ${
            builtins.concatStringsSep "\n" (
                map
                (pkg:
                    ''
                        echo "Adding ${pkg.packageResolvedFetched} to npm's cache"
                        npm cache add --loglevel-verbose ${pkg.packageResolvedFetched}
                    ''
                )
                packageLockDependencies)
        }

        #########################################################
        # Use `npm` to add the extra tarball dependencies from nix.
        #########################################################
        # Note `npm` needs to modify these files, so we change the permissions
        chmod +777 package.json
        chmod +777 package-lock.json

        ${ if dependencies == [] then "" else
            ''
                # We write the list of `dependencies` as 
                # `<dependency1>`, `<dependency2>`, ... ,`<dependencyN>`

                # Copying all `dependencies` into `.nix-nodes-deps/` i.e.,
                # we run:
                # ```
                # echo "Adding <dependency1> npm's cache"
                # ln -s <dependency1> .
                # npm cache add <dependency1>
                # echo "Adding <dependency2> npm's cache"
                # ln -s <dependency2> .
                # npm cache add <dependency2>
                # ...
                # echo "Adding <dependency2> npm's cache"
                # ln -s <dependencyN> .
                # npm cache add <dependencyN>
                # ```
                # TODO(jaredponn): Why are we symlinking the stuff in the local
                # directory and adding that to the cache? `npm` is a bit weird
                # when it comes to adding dependencies on the local
                # filesystem. It only allows relative paths, so by symlinking
                # everything in the project root, `npm` seems to be able to find
                # everything even when there are other npm modules which depend
                # on other local files.
                # This probably needs more investigation..
                ${builtins.concatStringsSep "\n" (builtins.map (pkgPath: 
                    ''
                        echo "Adding ${pkgPath} to npm's cache..."
                        ln -s "${pkgPath}" .
                        npm cache add --loglevel=verbose "$(basename "${pkgPath}")"
                    ''
                        ) dependencies)}

                # Run `npm install` with the previous dependencies i.e., we run
                # ```
                # npm install --save --package-lock-only <dependency1> <dependency2>  ... <dependencyN>
                # ```
                echo 'Running `npm --package-lock-only install` for the tarballs from nix...'
                npm install --loglevel=verbose --save --package-lock-only ${builtins.concatStringsSep " " (builtins.map (pkgPath: ''"$(basename "${pkgPath}")"'') dependencies) }
            ''
        }

        #########################################################
        # Install all the dependencies
        #########################################################
        echo 'Running `npm install` to create node_modules...'
        npm install

        #########################################################
        # Patch shebangs from the installed packages
        #########################################################

        for PKGJSON in $(find node_modules -name package.json -type f)
        do
            PKGDIR=$(dirname "$PKGJSON")
            for BIN in $(jq '(."bin"//{})[]' -r "$PKGJSON")
            do
                patchShebangs "$PKGDIR/$BIN"
            done
        done

        # Reset the permissions as they were i.e., read only for everyone
        chmod =444 package.json
        chmod =444 package-lock.json
      '';

  # Build the project (runs `npm run build`), and puts the entire output in the
  # nix store
  project =
    npmProject.overrideAttrs (_self: super:
      {
        # Append the build command at the end.
        buildCommand = super.buildCommand + "\n" +
          ''
            npm run --loglevel-verbose build
          '';
      });


  shell = pkgs.mkShell {
    packages = project.buildInputs ++ devShellTools;

    shellHook = ''
      export NODE_PATH=${npmProject}/node_modules/
      echo 'Creating a symbolic link from `$NODE_PATH` to `node_modules`...'

      ln -snf "$NODE_PATH" node_modules

      ${devShellHook}
    '';
  };

  # Creates a tarball of `project` using `npm pack` and puts it in the nix
  # store.
  npmPack = pkgs.stdenv.mkDerivation {
    buildInputs = [ nodejs ];
    name = "${name}.tgz";

    src = project;
    buildPhase = ''
      cp -r "$src/." .

      # Why do we set `HOME=$TMPDIR`? This is because apparently `npm` will
      # attempt to do something like `mkdir $HOME` for which `$HOME` is not
      # writable.. so we fix this by making `$HOME` a writable directory.
      tgzFile=$(HOME=$TMPDIR npm pack | tail -n 1)
    '';
    installPhase = ''
      mv "$tgzFile" "$out"
    '';

  };

  # Run tests with `npm test`.
  test = pkgs.stdenv.mkDerivation {
    buildInputs = project.buildInputs ++ testTools;
    name = "${name}-test";

    src = project;
    buildPhase = ''
      cp -r "$src/." .

      npm --loglevel=verbose test
    '';
    installPhase = ''
      touch $out
    '';

  };


in
{
  devShells = {
    "${name}-typescript" = shell;
  };

  packages = {
    "${name}-typescript" = project;
    "${name}-typescript-tgz" = npmPack;
    "${name}-typescript-npm-project" = npmProject;
  };

  checks = {
    "${name}-typescript-test" = test;
  };
}
