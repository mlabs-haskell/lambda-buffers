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
  packageJsonPath = "${src}/package.json";
  packageLockJsonPath = "${src}/package-lock.json";
  packageJson = builtins.fromJSON (builtins.readFile packageJsonPath);
  pname = packageJson.name;
  version = packageJson.version;

  # Derivation for the result of calling the CLI tool `node2nix` with the
  # provided `src`.
  #
  # Notes: 
  #
  #   - `node2nix` creates a nix expression in `default.nix` of type 
  #   `{pkgs, system, nodejs} -> {args, sources, tarball, package,  shell, nodeDependencies }` 
  #
  #   - the `shell` attribute in the output contains the folder
  #   `$out/lib/node_modules/` which is the `node_modules/` folder which node
  #   dependencies for node to run / develop with [this is very important]
  node2nixExprs =
    {
      # Extra flags passed directly to `node2nix`
      extraFlags ? [ ]
    }:
    pkgs.runCommand (name + "-node-packages") { buildInputs = [ pkgs.node2nix ]; }
      ''
        mkdir -p "$out"
        cd "$out"
        cp ${packageJsonPath} ./package.json
        cp ${packageLockJsonPath} ./package-lock.json
        node2nix --input package.json --lock package-lock.json ${builtins.concatStringsSep " " extraFlags}
      '';

  node2nixDevelopEnv = node2nixExprs { extraFlags = [ "--development" ]; };
  node2nixDevelopEnvShellNodeDependencies =
    ((import node2nixDevelopEnv) { inherit nodejs pkgs system; }).shell.nodeDependencies;

  # `mkNodeDerivation` wraps `stdenv.mkDerivation` but by default has:
  #   - Includes the attribute (environment variable) `NODE_MODULES` that is
  #   the `node_modules` folder for `node` to lookup the project's runtime
  #   dependencies 
  #   As such, the `buildPhase` attribute of `args` probably should have execute 
  #   `ln -s "$NODE_PATH" node_modules` s.t. when `node` can find all the
  #   dependencies when executed.
  #
  #   - adds a `preBuildPhase` which sets `NODE_PATH` to the `node_modules/`
  #   folder of `node2nixDevelopEnvShellNodeDependencies`.
  #
  #   - `nodejs` + `node2nixDevelopEnvShellNodeDependencies` in the
  #   `buildInputs`.
  #
  mkNodeDerivation = args: pkgs.stdenv.mkDerivation ({
    buildInputs = [ nodejs node2nixDevelopEnvShellNodeDependencies ];
    NODE_MODULES = "${node2nixDevelopEnvShellNodeDependencies}/lib/node_modules";
    preBuildPhase = ''
      export NODE_PATH="$NODE_MODULES"
    '';
  }
  // args);

  project = mkNodeDerivation {
    inherit pname version src;

    buildPhase = ''
      ln -s "$NODE_MODULES" node_modules

      npm run --logs-dir=. --loglevel=verbose build

      mkdir -p "$out"
      # Note: Why do we set `HOME=$PWD`? `npm` apparently likes to call `mkdir
      # $HOME` but it won't have permissions to write to the default `$HOME`
      # (`/homeless-shelter`). So, this fixes that.
      HOME="$PWD" npm pack --logs-dir=. --loglevel=verbose --pack-destination "$out"
    '';
  };

  tests = mkNodeDerivation {
    pname = pname + "-tests";
    inherit version src;

    buildPhase = ''
      ln -s "$NODE_MODULES" node_modules
      npm --logs-dir=. --loglevel=verbose test

      touch "$out"
    '';
  };

in
{
  devShells = {
    # Note: when using the devshell, when in the project root directory, type
    # ```
    # ln -s "$NODE_MODULES" node_modules
    # ```
    # so when `node` executes, it will use the dependencies gathered together
    # by nix
    "${name}" = project;
  };

  packages = {
    "${name}" = project;
    "${name}:node2nix" = node2nixDevelopEnv;
  };

  checks = {
    "${name}-test" = tests;
  };
}

