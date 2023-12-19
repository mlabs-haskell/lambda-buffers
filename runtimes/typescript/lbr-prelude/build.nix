{ ... }:
{
  perSystem = { config, ... }:
    let
      typescriptFlake =
        config.lbf-nix.typescriptFlake {
          name = "lbr-prelude";
          src = ./.;

          devShellTools = config.settings.shell.tools;
          devShellHook = config.settings.shell.hook;
          npmDepsHash  = "sha256-ribFu4BD7CcPXM68sFjQar4qB2ZDHddCnHNXTR4DJhk=";
        };
    in
    {
      inherit (typescriptFlake) packages checks devShells;
    };

}
