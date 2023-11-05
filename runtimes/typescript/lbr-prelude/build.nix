{ ... }:
{
  perSystem = { pkgs, ... }:
    let
      packagejson = builtins.fromJSON (builtins.readFile ./package.json);

      npmPackageAttr = {
        pname = packagejson.name;
        version = packagejson.version;

        # To update this, use `prefetch-npm-deps` to calculate the hash
        npmDepsHash = "sha256-d3ikFtNFzoMx7jyg3PyXniDUPD1fL99VbJDE660jnSc=";

        src = ./.;
      };

      project = pkgs.buildNpmPackage npmPackageAttr;
    in
    {
      devShells.dev-lbr-prelude-typescript = project;

      packages = {
        lbr-prelude-typescript = project;
      };

      checks = {
        lbr-prelude-typescript-tests = pkgs.buildNpmPackage (
          npmPackageAttr //
          {
            pname = npmPackageAttr.pname + "-test";
            buildPhase = ''
              npm test
              runHook postBuild
            '';
          }
        );
      };
    };
}
