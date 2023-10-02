{ ... }: {
  perSystem =
    { config
    , shellHook
    , ...
    }:
    let
      crateName = "lbr-prelude";
    in
    {
      nci.projects.${crateName}.path = ./.;
      nci.crates.${crateName} = { };
    };
}
