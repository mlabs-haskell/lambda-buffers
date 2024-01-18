pkgs:
let
  overrideAttrs = overrides: attrs:
    builtins.mapAttrs
      (name: val:
        if overrides?${name}
        then overrides.${name}.override val
        else val
      )
      ((builtins.mapAttrs (name: val: attrs.${name} or val.default) overrides) // attrs);
  # Makes a shell flag
  # Example: `mkFlag "foo" "bar" = "--foo='bar'"`
  mkFlag = key: value: "--${key}=${pkgs.lib.escapeShellArg "${value}"}";
  # Makes shell flags
  # Example: `mkFlags "foo" ["bar", "baz"] = "--foo='bar' --foo='baz'"`
  mkFlags = key: values: builtins.concatStringsSep " " (builtins.map (value: mkFlag key value) values);
  # `mkNestedFlags "foo" "bar" ["zim", "yom"] = "--foo='bar=\'zim\''" --foo='bar=\'yom\''`
  mkNestedFlags = key: nestedKey: values: builtins.concatStringsSep " " (builtins.map (value: "--${key}=${mkFlag key (mkFlag nestedKey value)}") values);
in
{ inherit overrideAttrs mkFlag mkFlags mkNestedFlags; }
