{
    runCommand, ...
}: 
{ 
    name,
    npmDependencies,
    ...
}: 
rec {

        npmLocalDependenciesDerivation = runCommand 
            "${name}-npm-local-dependencies" 
            { 
            }
            ''
                # Symlinks all the provided dependencies to $out
                mkdir -p $out
                cd $out
                ${ builtins.concatStringsSep "\n" (builtins.map (dep: ''ln -s "${dep}" .'') npmDependencies) }

                # TODO(jaredponn): not sure why but I think buildNpmPackage
                # will do weird things when a package depends on folder
                # dependencies i.e., if 
                #  - package A requires folder/A
                #  - package B requires A
                # when B requires A, it'll look in folder/folder/A instead of
                # just folder/A... so we create this cyclic symlink to resolve
                # this..

                echo "The dependencies are as follows"
                ls
            '';

        npmLocalDependenciesFolder = "./nix-npm-local-dependencies";

        npmLocalDependenciesLinkCommand = 
                '' 
                    echo 'Linking dependencies `${npmLocalDependenciesDerivation}` to `${npmLocalDependenciesFolder}`'
                    rm -rf ${npmLocalDependenciesFolder}
                    ln -sf "${npmLocalDependenciesDerivation}" ${npmLocalDependenciesFolder}
                '';
    }
