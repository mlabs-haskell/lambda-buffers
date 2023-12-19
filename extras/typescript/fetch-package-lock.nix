# Given a `package-lock.json`, grabs all packages as tarballs.
{ fetchurl, runCommand }:
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
      (
        builtins.map
          (packageLocation:
            {
              packageLocation = packageLocation;
              packageDescriptor = packagesAttrs.packageLocation;
              packageResolvedFetched =
                # TODO(jaredponn): I can't find a specification for the exact
                # format that they output but we try to generalize the examples in
                # [1]... at least we try to accept a hopefully reasonable superset
                # of what they output.
                # TODO(jaredponn): Urgh, we should really do things case insensitive
                let
                  resolvedStr = packagesAttrs."${packageLocation}".resolved;
                  # See here for the regex we steal:
                  # https://datatracker.ietf.org/doc/html/rfc3986
                  # In particular, see Appendix B: https://datatracker.ietf.org/doc/html/rfc3986#appendix-B
                  uriMatches = builtins.match ''^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?'' resolvedStr;

                in
                if builtins.elemAt uriMatches 1 == "https"
                then
                  fetchurl
                    {
                      url = resolvedStr;
                      hash = packagesAttrs."${packageLocation}".integrity;
                    }
                else 
                  if builtins.elemAt uriMatches  1 == "file"
                  then
                  # We skip files and assume they'll be provided
                  # separately.. 
                  # Fun fact: this is broken on `buildNpmPackages`
                    null

                  else
                    if builtins.elemAt uriMatches  1 == "git+ssh"
              then
              let gitUrl =
              builtins.concatStringsSep ""
              (
              builtins.filter (x: x != null)
              [ (builtins.elemAt uriMatches 0 )
              (builtins.elemAt uriMatches 2 )
              (builtins.elemAt uriMatches 4 )
              (builtins.elemAt uriMatches 5 )
              ]
              );
              in runCommand gitUrl {
                src = builtins.fetchGit 
                        { url = gitUrl;
                        rev = builtins.elemAt uriMatches 8 ;
                        };
            }
              ''
                cp -r ${src}/. $out
              ''


            else builtins.throw "Error: please report a bug. Unsupported `resolved` field in package-lock.json of `${resolvedStr}`";
  }
  )
  (builtins.attrNames packagesAttrs));

  in
  dependencies 
