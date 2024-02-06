# Catalyst milestone 4: Project adoption

## Outputs

- [x] Integration tooling for the build environment (Cabal, Spago. Nix).
  - LambdaBuffers team developed and provided their users with a set of Nix utility functions for building `.lbf` schemas into all the supported target language environments. Additionally, we extended the Nix support for working with Haskell and Purescript projects to allow for adding data and library dependencies which is crucial for adoption of any tool that leverages automated code generation.
  - The [LB Cardano Demo project](https://github.com/mlabs-haskell/lambdabuffers-cardano-demo) demonstrates the use of said Nix functions which results in a very concise and complete scaffold for any LambdaBuffers powered project.

   ```shell
   lambdabuffers-cardano-demo $ nix repl
   nix-repl> :lf .
   nix-repl> inputs.lbf.lib.x86_64-linux.
      inputs.lbf.lib.x86_64-linux.haskellData
      inputs.lbf.lib.x86_64-linux.haskellFlake
      inputs.lbf.lib.x86_64-linux.haskellPlutusFlake
      inputs.lbf.lib.x86_64-linux.lbfBuild
      inputs.lbf.lib.x86_64-linux.lbfHaskell
      inputs.lbf.lib.x86_64-linux.lbfPlutarch
      inputs.lbf.lib.x86_64-linux.lbfPlutarch'
      inputs.lbf.lib.x86_64-linux.lbfPlutusHaskell
      inputs.lbf.lib.x86_64-linux.lbfPlutusPurescript
      inputs.lbf.lib.x86_64-linux.lbfPreludeHaskell
      inputs.lbf.lib.x86_64-linux.lbfPreludePurescript
      inputs.lbf.lib.x86_64-linux.lbfPurescript
      inputs.lbf.lib.x86_64-linux.purescriptFlake
      inputs.lbf.lib.x86_64-linux.rustFlake
   ```

- [x] Continuous integration for regularly deploying toolkit packages to a package repository.
  - Hercules CI has been operating on the LambdaBuffers repo since the very start, and all the packages are readily available using Nix.
- [x] A Cardano dApp project partnership to help integrate the toolkit in their development and build environments.
  - The [LB Cardano Demo project](https://github.com/mlabs-haskell/lambdabuffers-cardano-demo) demonstrates the end to end Cardano dApp that uses LambdaBuffers and Plutus scripts written in both Plutarch and PlutusTx, whereas Cardano Transaction Library is used to construct transactions and test against the real network (using Plutip) that everything works as intended.
- [x] Documentation for integrating the build environment.
  - The [LB Cardano Demo project](https://github.com/mlabs-haskell/lambdabuffers-cardano-demo) demonstrates how to use LambdaBuffers and users are recommended to refer to that repo's README to help them integrate LambdaBuffers in their project.
  - Additionally, extensive LambdaBuffers documentation is made available at <https://mlabs-haskell.github.io/lambda-buffers/introduction.html> since the very start of the project.

## Acceptance Criteria

- [x] Toolkit packages are available in a package repository (eg. Nixpkgs, Hackage)
  - Bash shell is available in the repo that users can simply use to try out and use the LambdaBuffers toolkit.

   ```shell
   $ nix develop github:mlabs-haskell/lambda-buffers#lb
   $ lbf<TAB>
     lbf                        lbf-plutus-to-haskell      lbf-plutus-to-purescript   lbf-prelude-to-haskell     lbf-prelude-to-purescript  
   ```

- [x] Cardano dApp project maintains all the Plutus domain types in the configuration file and has fully equipped type libraries made available automatically via build environment integration tooling.
  - A demo project exists at <https://github.com/mlabs-haskell/lambdabuffers-cardano-demo> that showcase end to end use of the LambdaBuffers toolkit. The project defines the plutus and configuration API in `api` directory, the `validation` directory contains the same onchain script logic implemented using PlutusTx and Plutarch, the `transactions` directory contains transaction building logic using Cardano Transaction Library. All the devops is achieved using concise `build.nix` Nix based build recipes for each sub-project. Finally, the CI is also instructed to run the end to end test that tries out both Plutarch and PlutusTx scripts assuring it works as intended.

## Evidence of Milestone Completion

- [x] Completed and reviewed build environment integration tooling and source code are available in the project repository.
  - The [extras](./extras) contains all the Nix libraries the LB team developed to facilitate and streamline Cardano dApp development using LambdaBuffers and supported language ecosystems (Haskell, Purescript, Cardano Transaction Library, PlutusTx and Plutarch).
- [x] Toolkit packages are available in the package repository.
  - All LambdaBuffers tools are available using Nix.
- [x] Proof of use is provided by the partner dApp.
  - The [LB Cardano Demo project](https://github.com/mlabs-haskell/lambdabuffers-cardano-demo) demonstrates the end to end Cardano dApp that uses LambdaBuffers and Plutus scripts written in both Plutarch and PlutusTx, whereas Cardano Transaction Library is used to construct transactions and test against the real network (using Plutip) that everything works as intended.
- [x] Tooling use documentation is available in the project repository.
  - The [LB Cardano Demo project](https://github.com/mlabs-haskell/lambdabuffers-cardano-demo) demonstrates how to use LambdaBuffers and users are recommended to refer to that repo's README to help them integrate LambdaBuffers in their project.
  - Additionally, extensive LambdaBuffers documentation is made available at <https://mlabs-haskell.github.io/lambda-buffers/introduction.html> since the very start of the project.

## References

- [Catalyst project sheet](https://docs.google.com/spreadsheets/d/16dTxgGsxHvcMe5aCgFPDYEJKgX_VQiNAcwhp2RyA48o/edit#gid=1672366179)
