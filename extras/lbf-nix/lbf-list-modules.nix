{ lib }:
# root is the absolute path to the directory with .lbf files. 
# Example
# ~~~~~~
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

root:
# INVARIANT:
#   - dir == dotPrefix except dotPrefix doesn't contain the prefix root, and
#   dotPrefix has all `/`s replaced with a singular `.`
#   - `dotPrefix` always contains the dot at the end if it needs to be there
let
  matchLbfFile = name:
    let matches = builtins.match ''(.*)\.lbf$'' name;
    in if matches == null
    then null
    else builtins.elemAt matches 0;

  go = dir: dotPrefix:
    builtins.concatLists
      (lib.mapAttrsToList
        (name: value:
          if value == "directory"
          then go "${dir}/${name}" "${dotPrefix}${name}."

          else if value == "regular"
          then
          # Is this an lbf file?
            let lbfFileMatch = matchLbfFile name;
            in if lbfFileMatch != null
            then [ "${dotPrefix}${lbfFileMatch}" ]
            else [ ]
          else
            builtins.trace ''warning: `${name}` has file type `${value}` which is not a regular file nor directory''
              [ ]
        )
        (builtins.readDir dir)
      );
in
go root ""
