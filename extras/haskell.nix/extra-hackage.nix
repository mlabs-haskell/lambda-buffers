# Creates a haskell.nix module that adds the `extraHackage` options for specifying Cabal sources as additional compile dependencies.
compiler-nix-name:
let
  mylib = { pkgs, compiler-nix-name }: rec {
    mkPackageSpec = src:
      with pkgs.lib;
      let
        cabalFiles = concatLists (mapAttrsToList
          (name: type: if type == "regular" && hasSuffix ".cabal" name then [ name ] else [ ])
          (builtins.readDir src));

        cabalPath =
          if length cabalFiles == 1
          then src + "/${builtins.head cabalFiles}"
          else builtins.abort "Could not find unique file with .cabal suffix in source: ${src}";
        cabalFile = builtins.readFile cabalPath;
        parse = field:
          let
            lines = filter (s: if builtins.match "^${field} *:.*$" (toLower s) != null then true else false) (splitString "\n" cabalFile);
            line =
              if lines != [ ]
              then head lines
              else builtins.abort "Could not find line with prefix ''${field}:' in ${cabalPath}";
          in
          replaceStrings [ " " ] [ "" ] (head (tail (splitString ":" line)));
        pname = parse "name";
        version = parse "version";
      in
      { inherit src pname version; };

    mkHackageDirFor = { pname, version, src }:
      pkgs.runCommand "${pname}-${version}-hackage" { }
        ''
          set -e
          mkdir -p $out/${pname}/${version}
          md5=11111111111111111111111111111111
          sha256=1111111111111111111111111111111111111111111111111111111111111111
          length=1
          cat <<EOF > $out/"${pname}"/"${version}"/package.json
          {
            "signatures" : [],
            "signed" : {
                "_type" : "Targets",
                "expires" : null,
                "targets" : {
                  "<repo>/package/${pname}-${version}.tar.gz" : {
                      "hashes" : {
                        "md5" : "$md5",
                        "sha256" : "$sha256"
                      },
                      "length" : $length
                  }
                },
                "version" : 0
            }
          }
          EOF
          cp ${src}/*.cabal $out/"${pname}"/"${version}"/
        '';

    mkHackageTarballFromDirsFor = hackageDirs:
      let
        f = dir: ''
          echo ${dir}
          ln -s ${dir}/* hackage/
        '';
      in
      pkgs.runCommand "01-index.tar.gz" { } ''
        mkdir hackage
        ${builtins.concatStringsSep "" (map f hackageDirs)}
        cd hackage
        tar --sort=name --owner=root:0 --group=root:0 --mtime='UTC 2009-01-01' -hczvf $out */*/*
      '';

    mkHackageTarballFor = pkg-specs:
      mkHackageTarballFromDirsFor (map mkHackageDirFor pkg-specs);

    mkHackageNixFor = hackageTarball:
      pkgs.runCommand "hackage-nix" { } ''
        set -e
        export LC_CTYPE=C.UTF-8
        export LC_ALL=C.UTF-8
        export LANG=C.UTF-8
        cp ${hackageTarball} 01-index.tar.gz
        ${pkgs.gzip}/bin/gunzip 01-index.tar.gz
        ${pkgs.haskell-nix.nix-tools.${compiler-nix-name}}/bin/hackage-to-nix $out 01-index.tar "https://mkHackageNix/"
      '';

    copySrc = src: builtins.path {
      path = src;
      name = "copied-src-${builtins.baseNameOf (builtins.unsafeDiscardStringContext src)}";
    };

    mkModuleFor = pkg-specs: { lib, ... }: {
      # Prevent nix-build from trying to download the packages
      packages = pkgs.lib.listToAttrs (map
        (spec: {
          name = spec.pname;
          value = { src = lib.mkOverride 99 (copySrc spec.src); };
        })
        pkg-specs);
    };

    mkHackageFromSpecFor = pkg-specs: rec {
      extra-hackage-tarball = mkHackageTarballFor pkg-specs;
      extra-hackage = mkHackageNixFor extra-hackage-tarball;
      module = mkModuleFor pkg-specs;
    };

    mkHackageFor = srcs: mkHackageFromSpecFor (map mkPackageSpec srcs);
  };
in

{ lib, config, pkgs, ... }:
let
  l = mylib { inherit pkgs; inherit compiler-nix-name; };
  # FIXME: We have only one Hackage now
  # FIXME: Do copySrc here, but for some reason Nix shits itself
  theHackages = [ (l.mkHackageFor config.extraHackage) ];
  ifd-parallel = pkgs.runCommandNoCC "ifd-parallel" { myInputs = builtins.foldl' (b: a: b ++ [ a.extra-hackage a.extra-hackage-tarball ]) [ ] theHackages; } "echo $myInputs > $out";
  ifdseq = x: builtins.seq (builtins.readFile ifd-parallel.outPath) x;
  nlib = pkgs.lib;
in
{
  _file = "lambda-buffers/extras/haskell.nix/extra-hackage.nix";
  options = with lib.types; {
    extraHackage = lib.mkOption {
      type = listOf str; # FIXME: Allow passing in a tuple of the src and cabal file instead.
      default = [ ];
      description = "List of paths to cabal projects to include as extra hackages";
    };
  };
  config = lib.mkIf (config.extraHackage != [ ]) {
    modules = ifdseq (builtins.map (x: x.module) theHackages);
    extra-hackage-tarballs = ifdseq (
      nlib.listToAttrs (nlib.imap0
        (i: x: {
          name = "_" + builtins.toString i;
          value = x.extra-hackage-tarball;
        })
        theHackages));
    extra-hackages = ifdseq (builtins.map (x: import x.extra-hackage) theHackages);
  };
}
