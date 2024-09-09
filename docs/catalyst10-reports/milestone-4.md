# Catalyst milestone 4: Separate PlutusTx backend and improvements to existing LambdaBuffers facilities

## Outputs

- [x] A separate code generator for Haskell and PlutusTx

  - [x] A separate LambdaBuffers code generation module that outputs type definitions and derived implementations for Haskell's Prelude and PlutusTx's Prelude (or equivalent LambdaBuffers' Preludes) given a LambdaBuffers schema.

    - The common files for code generation of both Haskell's Prelude and PlutusTx's Prelude can be found [here](https://github.com/mlabs-haskell/lambda-buffers/tree/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/lambda-buffers-codegen/src/LambdaBuffers/Codegen/Haskell)

    - The Haskell code generator implementation can be found [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/lambda-buffers-codegen/src/LambdaBuffers/Codegen/Haskell.hs)

    - The PlutusTx code generator implementation can be found [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/lambda-buffers-codegen/src/LambdaBuffers/Codegen/PlutusTx.hs)

  - [x] A Haskell test suite that assures the manually implemented and automatically generated implementations are consistent with the predefined LambdaBuffers Prelude golden data set of JSON files and perform correct implementation derivation.

    - The Haskell test suite can be found [here](https://github.com/mlabs-haskell/lambda-buffers/tree/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/testsuites/lbt-prelude/lbt-prelude-haskell)

- [x] Nix devops modules (Nix API) for streamlining the LambdaBuffers code generation pipeline to either Haskell's Prelude or PlutusTx's Prelude.

  - A Nix API for streamlining LambdaBuffers code generation Haskell's Prelude can be found [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/extras/lbf-nix/build.nix#L17)

  - The Nix API for streamlining LambdaBuffers code generation PlutusTx's Prelude can be found [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/extras/lbf-nix/build.nix#L22)
    or PlutusTx's Prelude can be found

- [ ] Documentation on LambdaBuffers usage patterns for Haskell's Prelude and PlutusTx's Prelude
  
  - TODO

- [ ] A complete Plutus .lbf schema file to include all Plutus Ledger API types with backend support for Rust, TypeScript, and PureScript.

  - The `.lbf` schema file for V1 Plutus Ledger API can be found [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/libs/lbf-plutus/Plutus/V1.lbf)

  - The `.lbf` schema file for V2 Plutus Ledger API can be found [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/libs/lbf-plutus/Plutus/V2.lbf)

  - Rust backend support is given by the JSON file [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/lambda-buffers-codegen/data/rust-plutus-pla.json) where the types are provided by [this package](https://github.com/mlabs-haskell/plutus-ledger-api-rust).

  - TypeScript backend support is given by the JSON file [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/lambda-buffers-codegen/data/typescript-plutus.json) where the types are provided by [this package](https://github.com/mlabs-haskell/plutus-ledger-api-typescript)

  - PureScript backend support is given by the JSON file [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/lambda-buffers-codegen/data/purescript-plutus-ctl.json) where the types are provided by [this package](https://github.com/Plutonomicon/cardano-transaction-lib)

    TODO(jaredponn): this isn't implemented yet: see the "NotImplemented" in <https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/lambda-buffers-codegen/data/purescript-plutus-ctl.json#L123-L172>

- [x] An extended integration test suite to verify that the manually implemented and automatically implemented instances of the updated LambdaBuffers' Plutus .lbf schema file are consistent across all backends.

  - The extended integration test suite to verify that the manually implemented and automatically implemented instances of the updated LambdaBuffers' PlutusTx .lbf schema file are consistent across all backends can be found [here](https://github.com/mlabs-haskell/lambda-buffers/tree/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/testsuites/lbt-plutus).

- [x] A versioning scheme for the LambdaBuffers git repository using git tags that follow semantic versioning.

  - A versioning scheme for the git repository using git tags that follows semantic versioning is given in the [Releases page](https://github.com/mlabs-haskell/lambda-buffers/releases)

- [x] Optimized Nix build times.

  - This was fixed in [#233](https://github.com/mlabs-haskell/lambda-buffers/pull/233).
    It's well known that (in the version of Nix used with LambdaBuffers) [large `flake.lock` files are a detriment to performance](https://github.com/NixOS/nix/issues/6626).
    So prior of #233, in e.g. [9ae3705f3f1a5c2506c7f86c8d5643c38d79e849](https://github.com/mlabs-haskell/lambda-buffers/tree/9ae3705f3f1a5c2506c7f86c8d5643c38d79e849), we see that

    ```bash
    $ git checkout 9ae3705f3f1a5c2506c7f86c8d5643c38d79e849
    $ time nix flake lock

    real    0m27.437s
    user    0m27.331s
    sys     0m0.116s

    $ wc -l flake.lock
    44552 flake.lock
    ```

    it takes almost 28 seconds to execute the `nix flake lock` command due to the almost 45000 lines large `flake.lock` file.

    After the merge of #233, in e.g. [401f8a920a557c71440795174da199a1e128c4f9](https://github.com/mlabs-haskell/lambda-buffers/tree/401f8a920a557c71440795174da199a1e128c4f9), we see significantly improved Nix performance

    ```bash
    $ git checkout 401f8a920a557c71440795174da199a1e128c4f9
    $ time nix flake lock

    real    0m1.423s
    user    0m1.348s
    sys     0m0.067s

    $ wc -l flake.lock
    11585 flake.lock
    ```

    where it now takes only about 1.5 seconds to execute the `nix flake lock` command due to the significantly reduced `flake.lock` file size of being just under 12000 lines.

- [ ] Error messages that follow the GNU error message format.

## Acceptance Criteria

- [x] An executable and Nix tooling to translate LambdaBuffers modules to Haskell projects to specify application types.

  - An executable to translate LambdaBuffers modules to Haskell projects is given in the Nix attribute `lbf-prelude-to-haskell` defined [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/lambda-buffers-frontend/build.nix#L73-L86)
  
  - Convenient Nix tooling to translate LambdaBuffers modules to Haskell projects is given in the Nix function `lbfPreludeHaskell` defined here [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/extras/lbf-nix/build.nix#L17)

- [x] An executable and Nix tooling to translate LambdaBuffers modules to PlutusTx projects to specify application types.

  - An executable to translate LambdaBuffers modules to PlutusTx projects is given in the Nix attribute `lbf-plutus-to-plutustx` defined [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/lambda-buffers-frontend/build.nix#L124-L140)

  - Convenient Nix tooling to translate LambdaBuffers modules to PlutusTx projects is given in the Nix function `lbfPlutusTx` defined here [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/extras/lbf-nix/build.nix#L22)

- [x] An updated LambdaBuffers Plutus schema for the complete Plutus Ledger API types.

  - The updated LambdaBuffers Plutus schema for the complete Plutus Ledger API types can be found [here](https://github.com/mlabs-haskell/lambda-buffers/tree/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/libs/lbf-plutus/Plutus)

- [ ] The documentation and devops tooling is available to facilitate easy adoption.

  - Documentation can be found TODO(jaredponn).

  - Devops tooling can be found in the form of Nix functions for conveniently creating Haskell or PlutusTx projects found [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/extras/lbf-nix/build.nix#L14-L52).

- [x] Git tags for official releases of LambdaBuffers.

  - The [releases page](https://github.com/mlabs-haskell/lambda-buffers/releases) contains git tags for official releases of LambdaBuffers.

## Evidence of Milestone Completion

- [x] The completed and reviewed LambdaBuffers code generator for Haskell's Prelude.

  - The completed common files for code generation of both Haskell's Prelude and PlutusTx's Prelude can be found [here](https://github.com/mlabs-haskell/lambda-buffers/tree/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/lambda-buffers-codegen/src/LambdaBuffers/Codegen/Haskell)

  - The Haskell code generator implementation can be found [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/lambda-buffers-codegen/src/LambdaBuffers/Codegen/Haskell.hs)

- [x] The completed and reviewed LambdaBuffers code generator for PlutusTx's Prelude.

  - The completed common files for code generation of both Haskell's Prelude and PlutusTx's Prelude can be found [here](https://github.com/mlabs-haskell/lambda-buffers/tree/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/lambda-buffers-codegen/src/LambdaBuffers/Codegen/Haskell)

  - The PlutusTx code generator implementation can be found [here](https://github.com/mlabs-haskell/lambda-buffers/blob/2e2ff70f155ebcbac07b817f365f1220c24dfdf0/lambda-buffers-codegen/src/LambdaBuffers/Codegen/PlutusTx.hs)

- [x] Benchmarks of the before and after optimized Nix build times.

  - This was fixed in [#233](https://github.com/mlabs-haskell/lambda-buffers/pull/233), see above for benchmarks.

- [ ] Demonstrations (in the form of screenshots or simply text) of error messages following the GNU error message format.

## References

- [Project Catalyst Milestone 4](https://milestones.projectcatalyst.io/projects/1000122/milestones/4)
