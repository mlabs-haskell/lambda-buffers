# Catalyst milestone 2: End to end proof of concept

## Outputs

- [x] A configuration DSL for specifying domain data types.
  - LambdaBuffers **Frontend** supports specifying modules with **type definitions** with **opaque**, **product/record** and **sum** types. Additionally, type **class definitions** are supported as well as type **class rule definitions** using the 'instance clause' and 'derive' syntax.
  - Refer to the standard LambdaBuffers library [lbf-base](https://github.com/mlabs-haskell/lambda-buffers/tree/8b0900ebdd526ed042040d5bc9cef5a5b0281e98/experimental/lbf-base) to get a sense of what the language looks like.
- [x] A compiler tool that outputs the interpreted configuration.
  - The Compiler is exposed via an [Google Protobuffers based API](https://github.com/mlabs-haskell/lambda-buffers/blob/main/lambda-buffers-proto/compiler.proto). The Codegen shares the same API types and is exposed in a similar fashion. The Frontend communicates with these components via the API.
  - The API documentation is made available via [LambdaBuffers Github Pages](https://mlabs-haskell.github.io/lambda-buffers/compiler-api.html).
- [x] A Codegen module that takes in the interpreted configuration and outputs a Haskell+PlutusTx (was Plutarch) Cabal project containing all the types and necessary type class wiring.
  - The module implementation is in [lambda-buffers-codegen/src/LambdaBuffers/Codegen/Haskell](https://github.com/mlabs-haskell/lambda-buffers/tree/43d53222756e7b9bff836ec56e1a9b838678632e/lambda-buffers-codegen/src/LambdaBuffers/Codegen/Haskell).
  - The auto-generated Haskell files can be viewed in [lambda-buffers-codegen/data/goldens/haskell-autogen/LambdaBuffers](https://github.com/mlabs-haskell/lambda-buffers/tree/43d53222756e7b9bff836ec56e1a9b838678632e/lambda-buffers-codegen/data/goldens/haskell-autogen/LambdaBuffers).
  - Codegen module outputs Haskell type definitions, and [Prelude.Eq](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Eq.html#t:Eq) and [PlutusTx.ToData](https://github.com/input-output-hk/plutus/blob/848ec58de981a144226ee203c46144c0f3213f26/plutus-tx/src/PlutusTx/IsData/Class.hs#L34) type class implementations automatically.
  - Plutarch Codegen module is suspended for the current milestone (as communicated with the Catalyst team). Plutarch is a Haskell EDSL for writing Plutus (UPLC) programs, and can be considered a completely separate backend/target language.
- [x] A Codegen module that takes in the interpreted configuration and outputs a Purescript+CTL Spago project containing all the types and necessary wiring.
  - The module implementation is in [lambda-buffers-codegen/src/LambdaBuffers/Codegen/Purescript](https://github.com/mlabs-haskell/lambda-buffers/tree/43d53222756e7b9bff836ec56e1a9b838678632e/lambda-buffers-codegen/src/LambdaBuffers/Codegen/Purescript).
  - The auto-generated Purescript files can be viewed in [lambda-buffers-codegen/data/goldens/purescript-autogen/LambdaBuffers](https://github.com/mlabs-haskell/lambda-buffers/tree/43d53222756e7b9bff836ec56e1a9b838678632e/lambda-buffers-codegen/data/goldens/purescript-autogen/LambdaBuffers).
  - Codegen module outputs Purescript type definitions and [Prelude.Eq](https://pursuit.purescript.org/packages/purescript-prelude/3.1.0/docs/Data.Eq#t:Eq) and [Ctl.Internal.ToData
](https://github.com/Plutonomicon/cardano-transaction-lib/blob/b565f4b1ec877c671ec4ffc13b1b89dbe498bceb/src/Internal/ToData.purs#L73) type class implementations.

## Acceptance Criteria

- [x] The generated Haskell+Plutarch Cabal project can successfully be built.
- [x] The generated Purescript+CTL Spago project can successfully be built.
- [x] All the above codegen modules are reviewed and made available in the project repository.

## Evidence of Milestone Completion

- [x] Project repository is equipped with continuous integration tooling.
  - The repository has been consistently built with [HerculesCI](https://hercules-ci.com/) since the beginning.
- [x] Completed and reviewed Haskell+Plutarch codegen module is made available in the project repository and the CI builds it successfully.
  - The auto-generated Haskell files can be viewed in [lambda-buffers-codegen/data/goldens/haskell-autogen/LambdaBuffers](https://github.com/mlabs-haskell/lambda-buffers/tree/43d53222756e7b9bff836ec56e1a9b838678632e/lambda-buffers-codegen/data/goldens/haskell-autogen/LambdaBuffers).
  - Additionally, the auto generated Haskell files used during the demo can be found in [experimental/plutustx-env/autogen/LambdaBuffers](
https://github.com/mlabs-haskell/lambda-buffers/tree/8b0900ebdd526ed042040d5bc9cef5a5b0281e98/experimental/plutustx-env/autogen/LambdaBuffers).
  - Codegen module outputs Haskell type definitions, and [Prelude.Eq](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Eq.html#t:Eq) and [PlutusTx.ToData](https://github.com/input-output-hk/plutus/blob/848ec58de981a144226ee203c46144c0f3213f26/plutus-tx/src/PlutusTx/IsData/Class.hs#L34) type class implementations automatically.
  - Plutarch Codegen module is suspended for the current milestone (as communicated with the Catalyst team). Plutarch is a Haskell EDSL for writing Plutus (UPLC) programs, and can be considered a completely separate backend/target language.
- [x] Completed and reviewed Purescript+CTL codegen module is made available in the project repository and the CI builds it successfully.
  - The module implementation is in [lambda-buffers-codegen/src/LambdaBuffers/Codegen/Purescript](https://github.com/mlabs-haskell/lambda-buffers/tree/43d53222756e7b9bff836ec56e1a9b838678632e/lambda-buffers-codegen/src/LambdaBuffers/Codegen/Purescript).
  - The auto-generated Purescript files can be viewed in [lambda-buffers-codegen/data/goldens/purescript-autogen/LambdaBuffers](https://github.com/mlabs-haskell/lambda-buffers/tree/43d53222756e7b9bff836ec56e1a9b838678632e/lambda-buffers-codegen/data/goldens/purescript-autogen/LambdaBuffers).
  - Additionally, the auto generated Purescript files used during the demo can be found in [experimental/ctl-env/autogen/LambdaBuffers](
https://github.com/mlabs-haskell/lambda-buffers/tree/8b0900ebdd526ed042040d5bc9cef5a5b0281e98/experimental/ctl-env/autogen/LambdaBuffers).
  - Codegen module outputs Purescript type definitions and [Prelude.Eq](https://pursuit.purescript.org/packages/purescript-prelude/3.1.0/docs/Data.Eq#t:Eq) and [Ctl.Internal.ToData
](https://github.com/Plutonomicon/cardano-transaction-lib/blob/b565f4b1ec877c671ec4ffc13b1b89dbe498bceb/src/Internal/ToData.purs#L73) type class implementations.
- [x] Test case: Compiler is able to output a valid module with types from a schema in Haskell+Plutarch.
  - The auto-generated Haskell files can be viewed in [lambda-buffers-codegen/data/goldens/haskell-autogen/LambdaBuffers](https://github.com/mlabs-haskell/lambda-buffers/tree/43d53222756e7b9bff836ec56e1a9b838678632e/lambda-buffers-codegen/data/goldens/haskell-autogen/LambdaBuffers).
- [x] Test case: Compiler is able to output a valid module with types from a schema in PureScript.
  - The auto-generated Purescript files can be viewed in [lambda-buffers-codegen/data/goldens/purescript-autogen/LambdaBuffers](https://github.com/mlabs-haskell/lambda-buffers/tree/43d53222756e7b9bff836ec56e1a9b838678632e/lambda-buffers-codegen/data/goldens/purescript-autogen/LambdaBuffers).

## Demo recordings

- Introduction, working with the LambdaBuffers CLI tools and Frontend overview.
  - <https://www.youtube.com/watch?v=KnznwIdkFLM>
- Codegen into Haskell
  - <https://www.youtube.com/watch?v=WhZU66fcnig>
- Codegen into Purescript
  - <https://www.youtube.com/watch?v=tXnjt5h7D9w>

Demo files:

- LambdaBuffers [Citizen.lbf](https://github.com/mlabs-haskell/lambda-buffers/blob/8b0900ebdd526ed042040d5bc9cef5a5b0281e98/experimental/plutustx-env/api/Citizen.lbf) module.
- Purescript demo files [ctl-env](https://github.com/mlabs-haskell/lambda-buffers/tree/8b0900ebdd526ed042040d5bc9cef5a5b0281e98/experimental/ctl-env/autogen/LambdaBuffers).
- Haskell demo files [plutustx-env](https://github.com/mlabs-haskell/lambda-buffers/tree/8b0900ebdd526ed042040d5bc9cef5a5b0281e98/experimental/plutustx-env).

## References

- [Catalyst project sheet](https://docs.google.com/spreadsheets/d/16dTxgGsxHvcMe5aCgFPDYEJKgX_VQiNAcwhp2RyA48o/edit#gid=1672366179)
