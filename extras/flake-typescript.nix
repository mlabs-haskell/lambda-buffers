pkgs:
{ name
, src
, system
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
    pkgs.runCommand (name + "-node2nix") { buildInputs = [ pkgs.node2nix ]; }
      ''
        mkdir -p "$out"
        cd "$out"

        # In particular, we want
        # ```
        # cp ${packageJsonPath} ./package.json
        # cp ${packageLockJsonPath} ./package-lock.json
        # ```
        cp -r ${src}/* .

        if ! test -f package.json
        then { echo "No `package.json` provided"; exit 1; }
        fi

        if ! test -f package-lock.json
        then { echo "No `package-lock.json` provided"; exit 1; }
        fi

        node2nix --input package.json --lock package-lock.json ${builtins.concatStringsSep " " extraFlags}
      '';

  node2nixDevelop = node2nixExprs { extraFlags = [ "--development" ]; };
  node2nixDevelopAttrs = ((import node2nixDevelop) { inherit nodejs pkgs system; });

  # Build the project (runs `npm run build`)
  project = node2nixDevelopAttrs.package.override
    {
      postInstall =
        ''
          npm run --loglevel=verbose build
        '';
    };

  # Run tests
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
    "${name}" = node2nixDevelopAttrs.shell;
  };

  packages = {
    "${name}" = project;
    "${name}:node2nix" = node2nixDevelop;
  };

  checks = {
    "${name}:test" = test;
  };
}
