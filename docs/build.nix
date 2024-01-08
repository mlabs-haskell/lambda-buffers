_: {
  perSystem = { pkgs, config, ... }:
    {
      devShells.dev-docs = pkgs.mkShell {
        name = "docs-env";
        packages = [ pkgs.mdbook ];
        shellHook = config.settings.shell.hook;
      };

      packages.lambda-buffers-book = pkgs.stdenv.mkDerivation {
        src = ./.;
        name = "lambda-buffers-book";
        buildInputs = [ pkgs.mdbook ];
        buildPhase = ''
          cp ${config.packages.lambda-buffers-api-docs}/api.md api.md;
          mdbook build . --dest-dir $out
        '';
      };

    };
}
