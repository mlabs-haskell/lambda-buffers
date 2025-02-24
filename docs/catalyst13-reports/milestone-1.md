# Catalyst milestone 1: LambdaBuffers Conway support

## Outputs

Implementation in repository:

1. Implementing codegen for all modules (Haskell, Plutarch, PlutusTx, Rust, TypeScript, PureScript) for
   all the new [Plutus Ledger types](https://github.com/IntersectMBO/plutus/tree/master/plutus-ledger-api/src/PlutusLedgerApi/V3).
   This includes the new governance-related types, delegation types, and updated transaction types.
2. Create new golden tests for the above types, for all language modules. For instance, we will have a
   set of files consisting of known-good JSON-serialized instances, and our testsuite will check that
   our implementation outputs these serialized values upon execution as expected. Although an exhaustive
   list of tests is not possible here, there are many Plutus types updated or new in V3 our library will
   support such as Vote, Voter, TxOutRef, TxInInfo, TxCert, etc and various concrete instantiations of
   each type to test - e.g., a VoteNo Vote, a VoteYes Vote, and so on.
3. Create new roundtrip tests for Eq, Json and PlutusData type classes (where applicable) that will
   verify our type class instances serialise and deserialize without corruption, loss of information,
   etc by converting values to JSON and back, PlutusData and back, and verifying equality withstands.
4. Implement new Plutus V3 types in plutus-ledger-api-rust and plutus-ledger-api-typescript
5. Documentation explaining the updates accomplished towards LambdaBuffers Conways support

## Acceptance Criteria

1. All Plutus V3 types are implemented correctly and LambdaBuffers now supports codegen for all the
   supported language modules for these types:

   - Both the Plutus Ledger APIs and codegen configurations are implemented
     ensuring execution outputs are as intended

2. All Plutus V3 types are covered by the golden tests ensuring execution outputs are as intended

   - All languages use the same set of golden files to test against

3. All Plutus V3 have roundtrip tests for Eq, Json and PlutusData type classes (where applicable)

   - All languages have roundtrip tests (where applicable) to tests Json and PlutusData serialization

4. The output is suitable for Plutus V3 Types such as Voter, Delegatee and GovernanceActionID etc.

   - All new Plutus V3 types have been implemented in the supporting libraries

5. Documentation will cover new type support across Lambda buffer supported languages and will
   appear on [https://mlabs-haskell.github.io/lambda-buffers](https://mlabs-haskell.github.io/lambda-buffers/) in the Catalyst Milestone F13 reporting.

   - Documentation of the newly implemented Plutus V3 types were auto-generated

## Evidence of Milestone Completion

- Proof of Achievement 1: Codegen implementation will be added to the github.com/mlabs-haskell/lambda-buffers repository and a link will be provided.
  - Haskell codegen config: [lambda-buffers-codegen/data/haskell-plutus-plutustx.json](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/lambda-buffers-codegen/data/haskell-plutus-plutustx.json)
  - Plutarch codegen config: [lambda-buffers-codegen/data/plutarch-plutus.json](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/lambda-buffers-codegen/data/plutarch-plutus.json)
  - PlutusTx codegen config: [lambda-buffers-codegen/data/plutustx-plutus.json](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/lambda-buffers-codegen/data/plutustx-plutus.json)
  - Rust codegen config: [lambda-buffers-codegen/data/rust-plutus-pla.json](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/lambda-buffers-codegen/data/rust-plutus-pla.json)
  - TypeScript codegen config: [lambda-buffers-codegen/data/typescript-plutus.json](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/lambda-buffers-codegen/data/typescript-plutus.json)
  - PureScript codegen config: [lambda-buffers-codegen/data/purescript-plutus-ctl.json](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/lambda-buffers-codegen/data/purescript-plutus-ctl.json)
- Proof of Achievement 2: Rust plutus-ledger-api types and golden tests will be implemented in
  [https://github.com/mlabs-haskell/plutus-ledger-api-rust](https://github.com/mlabs-haskell/plutus-ledger-api-rust) and a link will be provided.
  - Golden tests for V3 types can be found here:
    - Haskell: [testsuites/lbt-plutus/lbt-plutus-haskell/src/Test/LambdaBuffers/Plutus/Golden.hs](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/testsuites/lbt-plutus/lbt-plutus-haskell/src/Test/LambdaBuffers/Plutus/Golden.hs)
    - Plutarch: [testsuites/lbt-plutus/lbt-plutus-plutarch/test/Test/LambdaBuffers/Runtime/Plutarch/PlutusData.hs](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/testsuites/lbt-plutus/lbt-plutus-plutarch/test/Test/LambdaBuffers/Runtime/Plutarch/PlutusData.hs)
    - PlutusTx: [testsuites/lbt-plutus/lbt-plutus-plutustx/test/Test/LambdaBuffers/Runtime/PlutusTx/PlutusTx.hs](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/testsuites/lbt-plutus/lbt-plutus-plutustx/test/Test/LambdaBuffers/Runtime/PlutusTx/PlutusTx.hs)
    - Rust: [testsuites/lbt-plutus/lbt-plutus-rust/tests/goldens.rs](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/testsuites/lbt-plutus/lbt-plutus-rust/tests/goldens.rs)
    - TypeScript: [testsuites/lbt-plutus/lbt-plutus-typescript/src/Goldens.ts](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/testsuites/lbt-plutus/lbt-plutus-typescript/src/Goldens.ts)
- Proof of Achievement 3: Plutus V3 roundtrip tests located at [https://github.com/mlabs-haskell/lambda-buffers](https://github.com/mlabs-haskell/lambda-buffers) repo and a link will be provided.
  - Roundtrip tests were implemented and can be found here:
    - Haskell
      - PlutusData: [https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/testsuites/lbt-plutus/lbt-plutus-haskell/test/Test/LambdaBuffers/Runtime/Plutus/PlutusData.hs](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/testsuites/lbt-plutus/lbt-plutus-haskell/test/Test/LambdaBuffers/Runtime/Plutus/PlutusData.hs)
      - Json: [https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/testsuites/lbt-plutus/lbt-plutus-haskell/test/Test/LambdaBuffers/Runtime/Plutus/Json.hs](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/testsuites/lbt-plutus/lbt-plutus-haskell/test/Test/LambdaBuffers/Runtime/Plutus/Json.hs)
    - Plutarch: [testsuites/lbt-plutus/lbt-plutus-plutarch/test/Test/LambdaBuffers/Runtime/Plutarch/PlutusData.hs](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/testsuites/lbt-plutus/lbt-plutus-plutarch/test/Test/LambdaBuffers/Runtime/Plutarch/PlutusData.hs)
    - PlutusTx: [testsuites/lbt-plutus/lbt-plutus-plutustx/test/Test/LambdaBuffers/Runtime/PlutusTx/PlutusTx.hs](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/testsuites/lbt-plutus/lbt-plutus-plutustx/test/Test/LambdaBuffers/Runtime/PlutusTx/PlutusTx.hs)
    - Rust: [testsuites/lbt-plutus/lbt-plutus-rust/tests/main.rs](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/testsuites/lbt-plutus/lbt-plutus-rust/tests/main.rs)
    - TypeScript
      - PlutusData: [testsuites/lbt-plutus/lbt-plutus-typescript/src/PlutusData-test.ts](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/testsuites/lbt-plutus/lbt-plutus-typescript/src/PlutusData-test.ts)
      - Json: [testsuites/lbt-plutus/lbt-plutus-typescript/src/Json-test.ts](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/testsuites/lbt-plutus/lbt-plutus-typescript/src/Json-test.ts)
- Proof of Achievement 4: Plutus V3 types located at [https://github.com/mlabs-haskell/plutus-ledger-api-rust](https://github.com/mlabs-haskell/plutus-ledger-api-rust)
  [https://github.com/mlabs-haskell/plutus-ledger-api-typescript](https://github.com/mlabs-haskell/plutus-ledger-api-typescript) and links provided.
  - Plutus_Ledger API Rust implementation was implemented here: [mlabs-haskell/plutus-ledger-api-rust v3.0.2](https://github.com/mlabs-haskell/plutus-ledger-api-rust/tree/v3.0.2/plutus-ledger-api/src/v3)
  - Plutus_Ledger API TypeScript implemnentation was implemented here: [mlabs-haskell/plutus-ledger-api-typescript v1.2.1](https://github.com/mlabs-haskell/plutus-ledger-api-typescript/tree/v1.2.1/src/Lib/V3)
- Proof of Achievement 5: Page addressing the new Documentation on types for Catalyst Fund 13 at [https://mlabs-haskell.github.io/lambda-buffers](https://mlabs-haskell.github.io/lambda-buffers)
  - Documentation of plutus-ledger-api-rust V3 types is located here: [https://mlabs-haskell.github.io/plutus-ledger-api-rust/plutus_ledger_api/v3](https://mlabs-haskell.github.io/plutus-ledger-api-rust/plutus_ledger_api/v3)
  - Documentation of plutus-ledger-api-typescript V3 types is located here: [https://mlabs-haskell.github.io/plutus-ledger-api-typescript/modules/plutus_ledger_api_V3_js](https://mlabs-haskell.github.io/plutus-ledger-api-typescript/modules/plutus_ledger_api_V3_js.html)
  - Conway compatibility report [docs/catalyst13-reports/milestone-1.md](https://github.com/mlabs-haskell/lambda-buffers/tree/v2.0.0/docs/catalyst13-reports/milestone-1.md)
