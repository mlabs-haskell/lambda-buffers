{ runCommand
, ...
}:
{ name
, npmDependencies
, ...
}:
rec {

  npmLocalDependenciesDerivation = runCommand
    "${name}-npm-local-dependencies"
    { }
    ''
      # Symlinks all the provided dependencies to $out
      mkdir -p $out
      cd $out
      ${ builtins.concatStringsSep "\n" (builtins.map (dep: ''ln -sf "${dep}/tarballs/"* .'') npmDependencies) }
    '';

  npmLocalDependenciesFolder = "./nix-tarballs";

  npmLocalDependenciesLinkCommand =
    '' 
                    echo 'Linking dependencies `${npmLocalDependenciesDerivation}` to `${npmLocalDependenciesFolder}`'
                    rm -rf ${npmLocalDependenciesFolder}
                    ln -sf "${npmLocalDependenciesDerivation}" ${npmLocalDependenciesFolder}
                '';
}
