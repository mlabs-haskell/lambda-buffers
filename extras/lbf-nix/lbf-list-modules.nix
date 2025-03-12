# Provides a nix function which recursively lists all lbf modules in a
# directory.
#
# Type: { lib : attr } -> path -> [string]
#
# Example:
# ```nix
# let
#   lbfListModules = pkgs.callPackage (import ./lbf-list-modules.nix) {};
# in lbfListModules ./src
# ```
# evaluates to
# ```
# MySchema
# Directory.MySchema
# ```
# where
# ```text
# $ tree src/
# src/
# ├── Directory
# │   └── MySchema.lbf
# └── MySchema.lbf
# ```

{ lib }:
# root is the absolute path to the directory with .lbf files.
root:
let
  # Given `<name>.lbf`, this returns `<name>` returning `null` otherwise.
  # Type: matchLbfFile :: string -> string | null
  matchLbfFile =
    name:
    let
      matches = builtins.match ''(.*)\.lbf$'' name;
    in
    if matches == null then null else builtins.elemAt matches 0;

  go =
    dir:
    builtins.concatLists (
      lib.mapAttrsToList (
        name: value:
        if value == "directory" then
          builtins.map (suffix: "${name}.${suffix}") (go "${dir}/${name}")
        else if value == "regular" then
          # Is this an lbf file?
          let
            lbfFileMatch = matchLbfFile name;
          in
          if lbfFileMatch != null then [ lbfFileMatch ] else [ ]
        else
          builtins.trace
            ''warning: `${name}` has file type `${value}` which is not a regular file nor directory''
            [ ]
      ) (builtins.readDir dir)
    );
in
go root
