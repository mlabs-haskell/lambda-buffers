{
  overrideAttrs = overrides: attrs:
    builtins.mapAttrs
      (name: val:
        if overrides?${name}
        then overrides.${name}.override val
        else val
      )
      ((builtins.mapAttrs (name: val: if attrs?${name} then attrs.${name} else val.default) overrides) // attrs);
  mkFlags = flag: xs: builtins.concatStringsSep " " (builtins.map (x: "--${flag} ${x}") xs);
}
