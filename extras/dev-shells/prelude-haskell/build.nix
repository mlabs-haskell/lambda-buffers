{ inputs, ... }:
{
  perSystem =
    { config, system, ... }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellFlake {
        src = ./.;

        name = "prelude-haskell";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          "${config.packages.lbr-prelude-haskell-src}"
          "${config.packages.lbf-prelude-haskell}"
        ];

        devShellTools = config.settings.shell.tools ++ [ config.packages.lbf-prelude-to-haskell ];
        devShellHook = config.settings.shell.hook;
      };
    in
    {
      # Develop Prelude applications with Haskell
      devShells.dev-prelude-haskell = hsFlake.devShells.default;
      packages.play-prelude-haskell-lib = hsFlake.packages."prelude-haskell:lib:prelude-haskell";
    };
}
