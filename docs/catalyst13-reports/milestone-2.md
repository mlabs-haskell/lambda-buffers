# Catalyst milestone 2: LambdaBuffers Conway support

## Outputs

Implementation in repository:

    - [x] Implementing codegen for all modules (Haskell, Plutarch, PlutusTx, Rust, TypeScript, PureScript) for all the new Plutus Ledger types https://github.com/IntersectMBO/plutus/tree/master/plutus-ledger-api/src/PlutusLedgerApi/V3
        - Haskell codegen config: [lambda-buffers-codegen/data/haskell-plutus-plutustx.json](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/lambda-buffers-codegen/data/haskell-plutus-plutustx.json)
        - Plutarch codegen config: [lambda-buffers-codegen/data/plutarch-plutus.json](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/lambda-buffers-codegen/data/plutarch-plutus.json)
        - PlutusTx codegen config: [lambda-buffers-codegen/data/plutustx-plutus.json](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/lambda-buffers-codegen/data/plutustx-plutus.json)
        - Rust codegen config: [lambda-buffers-codegen/data/rust-plutus-pla.json](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/lambda-buffers-codegen/data/rust-plutus-pla.json)
        - TypeScript codegen config: [lambda-buffers-codegen/data/typescript-plutus.json](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/lambda-buffers-codegen/data/typescript-plutus.json)
        - PureScript codegen config: [lambda-buffers-codegen/data/purescript-plutus-ctl.json](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/lambda-buffers-codegen/data/purescript-plutus-ctl.json)
    - [x] Create new golden tests for the above types, for all language modules
        - Golden tests for V3 types can be found here:
            - Haskell: [testsuites/lbt-plutus/lbt-plutus-haskell/src/Test/LambdaBuffers/Plutus/Golden.hs](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/testsuites/lbt-plutus/lbt-plutus-haskell/src/Test/LambdaBuffers/Plutus/Golden.hs)
            - Plutarch: [testsuites/lbt-plutus/lbt-plutus-plutarch/test/Test/LambdaBuffers/Runtime/Plutarch/PlutusData.hs](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/testsuites/lbt-plutus/lbt-plutus-plutarch/test/Test/LambdaBuffers/Runtime/Plutarch/PlutusData.hs)
            - PlutusTx: [testsuites/lbt-plutus/lbt-plutus-plutustx/test/Test/LambdaBuffers/Runtime/PlutusTx/PlutusTx.hs](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/testsuites/lbt-plutus/lbt-plutus-plutustx/test/Test/LambdaBuffers/Runtime/PlutusTx/PlutusTx.hs)
            - Rust: (testsuites/lbt-plutus/lbt-plutus-rust/tests/goldens.rs)[https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/testsuites/lbt-plutus/lbt-plutus-rust/tests/goldens.rs]
            - TypeScript: [testsuites/lbt-plutus/lbt-plutus-typescript/src/Goldens.ts](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/testsuites/lbt-plutus/lbt-plutus-typescript/src/Goldens.ts)
            - PureScript
    - [x] Create new roundtrip tests for Eq, Json and PlutusData type classes (where applicable)
        - Roundtrip tests were implemented and can be found here:
            - Haskell
                - PlutusData: [https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/testsuites/lbt-plutus/lbt-plutus-haskell/test/Test/LambdaBuffers/Runtime/Plutus/PlutusData.hs](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/testsuites/lbt-plutus/lbt-plutus-haskell/test/Test/LambdaBuffers/Runtime/Plutus/PlutusData.hs)
                - Json: [https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/testsuites/lbt-plutus/lbt-plutus-haskell/test/Test/LambdaBuffers/Runtime/Plutus/Json.hs](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/testsuites/lbt-plutus/lbt-plutus-haskell/test/Test/LambdaBuffers/Runtime/Plutus/Json.hs)
            - Plutarch: [testsuites/lbt-plutus/lbt-plutus-plutarch/test/Test/LambdaBuffers/Runtime/Plutarch/PlutusData.hs](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/testsuites/lbt-plutus/lbt-plutus-plutarch/test/Test/LambdaBuffers/Runtime/Plutarch/PlutusData.hs)
            - PlutusTx: [testsuites/lbt-plutus/lbt-plutus-plutustx/test/Test/LambdaBuffers/Runtime/PlutusTx/PlutusTx.hs](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/testsuites/lbt-plutus/lbt-plutus-plutustx/test/Test/LambdaBuffers/Runtime/PlutusTx/PlutusTx.hs)
            - Rust: [testsuites/lbt-plutus/lbt-plutus-rust/tests/main.rs](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/testsuites/lbt-plutus/lbt-plutus-rust/tests/main.rs)
            - TypeScript
                - PlutusData: [testsuites/lbt-plutus/lbt-plutus-typescript/src/PlutusData-test.ts](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/testsuites/lbt-plutus/lbt-plutus-typescript/src/PlutusData-test.ts) 
                - Json: [testsuites/lbt-plutus/lbt-plutus-typescript/src/Json-test.ts](https://github.com/mlabs-haskell/lambda-buffers/blob/c3d59cfdb6463c2614292e1bb9b81cfcae893018/testsuites/lbt-plutus/lbt-plutus-typescript/src/Json-test.ts) 
    - [x] Implement new Plutus V3 types in plutus-ledger-api-rust and plutus-ledger-api-typescript
        - Plutus_Ledger API Rust implementation was implemented here: [mlabs-haskell/plutus-ledger-api-rust v3.0.2](https://github.com/mlabs-haskell/plutus-ledger-api-rust/tree/v3.0.2/plutus-ledger-api/src/v3)
        - Plutus_Ledger API TypeScript implemnentation was implemented here: [mlabs-haskell/plutus-ledger-api-typescript v1.2.0](https://github.com/mlabs-haskell/plutus-ledger-api-typescript/tree/v1.2.0/src/Lib/V3)

## Acceptance Criteria

    - [x] All Plutus V3 types are implemented
        - Both the Plutus Ledger APIs and codegen configurations are implemented
    - [x] All Plutus V3 types are covered by the golden tests
        - All languages use the same set of golden files to test against
    - [x] All Plutus V3 have roundtrip tests for Eq, Json and PlutusData type classes (where applicable)
        - All languages have roundtrip tests (where applicable) to tests Json and PlutusData serialization

## Evidence of Milestone Completion

    - [x] Codegen implementation will be added to the github.com/mlabs-haskell/lambda-buffers repository.
    - [x] Rust plutus-ledger-api types will be implemented in https://github.com/mlabs-haskell/plutus-ledger-api-rust .
    - [x] TypeScript plutus-ledger-api types will be implemented in https://github.com/mlabs-haskell/plutus-ledger-api-typescript .
