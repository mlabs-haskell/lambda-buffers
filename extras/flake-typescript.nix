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
  # Loosely, this will `npm install` each of the tarballs in order so its
  # important that
  # 
  #     - The dependencies are sorted topologically i.e., each tarball should
  #     _only_ depend on packages before it in the list.
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
, ...
}:
let
  # Derivation for the result of calling the CLI tool `node2nix` with the
  # provided `src`.
  #
  # Notes: 
  #
  #   - `node2nix` creates a nix expression in `default.nix` of type 
  #   `{pkgs, system, nodejs} -> {args, sources, tarball, package,  shell, nodeDependencies }` 
  node2nixExprs =
    {
      # Extra flags passed directly to `node2nix`
      extraFlags ? [ ]
    }:
    pkgs.runCommand (name + "-node2nix") { buildInputs = [ pkgs.node2nix nodejs ]; }
      ''
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
        # Use `npm` to add the extra tarball dependencies from nix.
        #########################################################

        # Note `npm` needs to modify these files, so we change the permissions
        chmod +777 package.json
        chmod +777 package-lock.json

        # `.nix-node-deps/` is the directory to save the tarball
        # dependencies from `nix`.
        mkdir .nix-node-deps/

        ${ if dependencies == [] then "" else
            ''
                # We write the list of `dependencies` as 
                # `<dependency1>`, `<dependency2>`, ... ,`<dependencyN>`

                # Copying all `dependencies` into `.nix-nodes-deps/` i.e.,
                # we run:
                # ```
                # echo "Copying <dependency1>"
                # cp Copying <dependency1> .nix-node-deps/
                # echo "Copying <dependency2>"
                # cp Copying <dependency2> .nix-node-deps/
                # ...
                # echo "Copying <dependencyN>"
                # cp Copying <dependencyN> .nix-node-deps/
                # ```
                ${builtins.concatStringsSep "\n" (builtins.map (pkgPath: 
                    ''
                        echo "Copying ${pkgPath}..."
                        cp "${pkgPath}" .nix-node-deps/
                    ''
                        ) dependencies)}

                # Run `npm install` with the previous dependencies i.e., we run
                # ```
                # npm install --save --package-lock-only <dependency1> <dependency2>  ... <dependencyN>
                # ```
                echo 'Running `npm install`...'
                HOME=$TMPDIR npm install --save --package-lock-only ${builtins.concatStringsSep " " (builtins.map (pkgPath: ''".nix-node-deps/$(basename "${pkgPath}")"'') dependencies) }
            ''
        }

        #########################################################
        # Run `node2nix`
        #########################################################
        echo 'Running `node2nix`...'
        node2nix --input package.json --lock package-lock.json ${builtins.concatStringsSep " " extraFlags}

        # Reset the permissions for  `package.json` and `package-lock.json` to
        # read only for everyone.
        chmod =444 package.json
        chmod =444 package-lock.json
      '';

  node2nixDevelop = node2nixExprs { extraFlags = [ "--development" ]; };
  node2nixDevelopAttrs = ((import node2nixDevelop) { inherit nodejs pkgs; inherit (pkgs) system; });

  # Build the project (runs `npm run build`), and puts the entire output in the
  # nix store
  project = node2nixDevelopAttrs.package.override
    {
      postInstall =
        ''
          npm run --loglevel=verbose build
        '';
    };

  shell = node2nixDevelopAttrs.shell.override
    {
      shellHook =
        node2nixDevelopAttrs.shell.shellHook
        +
        ''
          # Note: `node2nix` sets `$NODE_PATH` s.t. it is a single path.
          echo 'Creating a symbolic link from `$NODE_PATH` to `node_modules`...'
          ln -snf "$NODE_PATH" node_modules
        '';
    };

  # Creates a tarball of `project` using `npm pack` and puts it in the nix
  # store.
  tarball = pkgs.stdenv.mkDerivation {
    buildInputs = [ nodejs ];
    name = "${node2nixDevelopAttrs.args.name}.tgz";

    # A glance at the generated nix expressions in `node2nixDevelop` will
    # show why the following path is where the package really is.
    src = "${project}/lib/node_modules/${node2nixDevelopAttrs.args.packageName}";
    buildPhase = ''
      cp -r "$src" .

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
  test = node2nixDevelopAttrs.package.override
    {
      postInstall =
        ''
          npm --loglevel=verbose test
          rm -rf $out
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
    "${name}-typescript-tarball" = tarball;
    "${name}-typescript-node2nix" = node2nixDevelop;
  };

  checks = {
    "${name}-typescript-test" = test;
  };
}
