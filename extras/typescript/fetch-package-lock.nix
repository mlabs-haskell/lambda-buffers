# Given a `package-lock.json`, grabs all packages as tarballs.
pkgs:
{ src ? ./.
, packageLock ? "${src}/package-lock.json"
, ...
}:
let
  # The plan.
  # - The `package-lock.json` file contains a `packages` key which maps to an
  # object that maps package locations to a package descriptors (object
  # containing information about that package)
  # - Those objects have as package descriptors (objects) containing
  #   - `resolved`: the place where the package was actually resolved from e.g. 
  #           - url to tarball
  #           - git URL with commit SHA,
  #           - location of link target
  #   - `integrity`: A sha512 or sha1 Standard Subresouce Integrity string
  #   for the artifact that was unpacked in this location
  # See [1] for more details.
  # - We're going to take all of those things, and fetch the tarball
  # ourselves manually, and create a nix derivation for each of the
  # dependencies.
  # 
  # References.
  #   [1] https://docs.npmjs.com/cli/v10/configuring-npm/package-lock-json

  packageLockAttrs = builtins.fromJSON (builtins.readFile packageLock);
  # Note we remove the package location "" because that's the root project
  packagesAttrs =
    builtins.removeAttrs
      (packageLockAttrs.packages or  { })
      [ "" ];

  # The type should be 
  # [ { packageLocation : string, packageDescriptor: set, packageResolvedFetched  } ]
  dependencies =
    builtins.map
      (packageLocation:
        {
          packageLocation = packageLocation;
          packageDescriptor = packagesAttrs.packageLocation;
          packageResolvedFetched =
            # TODO(jaredponn): Broken for non URL dependencies
            pkgs.fetchurl {
              url = packagesAttrs."${packageLocation}".resolved;
              hash = packagesAttrs."${packageLocation}".integrity;
            };
        }
      )
      (builtins.attrNames packagesAttrs);

in
dependencies 
