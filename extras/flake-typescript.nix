pkgs:
{ name
, src
, system
, # `dependencies` is of type
  # ```
  # [ nix derivation for a tarball from `npm pack` ]
  # ```
  # for the the extra dependencies (not included in the `package.json`) for
  # `node` to execute.
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

, # TODO(jaredponn): Why do we use such an old version of node?
  # This node version uses npm 8.19.3 which generates `"lockfileVersion": 2`
  # in the `package-lock.json` which is compatible with `node2nix`; and
  # routine verification will show that newer versions of node (npm) use
  # `"lockfileVersion": 3` which is incompatible with `node2nix`... 
  # So to fix this, we could either patch `node2nix` or roll our own
  # thing that scans lock files / fetches the tarballs. The latter really
  # doesn't seem that hard to do...
  nodejs ? pkgs.nodejs-16_x
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
        then { echo "No `package.json` provided"; exit 1; }
        fi

        if ! test -f package-lock.json
        then { echo "No `package-lock.json` provided. Running `npm install --package-lock-only` may fix this"; exit 1; }
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

        ${builtins.concatStringsSep "\n"
            ( builtins.map
                (pkgPath: 
                    ''
                        PKG="${pkgPath}"
                        cp "$PKG" .nix-node-deps/
                        HOME=$TMPDIR npm install --save --package-lock-only ".nix-node-deps/$(basename "$PKG")"
                    ''
                )
                dependencies
            )

        }

        # Reset the permissions for  `package.json` and `package-lock.json` to
        # read only for everyone.
        chmod =444 package.json
        chmod =444 package-lock.json

        #########################################################
        # Run `node2nix`
        #########################################################
        node2nix --input package.json --lock package-lock.json ${builtins.concatStringsSep " " extraFlags}
      '';

  node2nixDevelop = node2nixExprs { extraFlags = [ "--development" ]; };
  node2nixDevelopAttrs = ((import node2nixDevelop) { inherit nodejs pkgs system; });

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
    { };

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
    # Note: when using the devshell, when in the project root directory, type
    # ```
    # ln -s "$NODE_PATH" node_modules
    # ```
    # so when `node` executes, it will use the dependencies gathered together
    # by nix
    "${name}" = shell;
  };

  packages = {
    "${name}" = project;
    "${name}-tarball" = tarball;
    "${name}-node2nix" = node2nixDevelop;
  };

  checks = {
    "${name}-test" = test;
  };
}
