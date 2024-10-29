# Catalyst milestone 5 (FINAL): Project scaffold for Rust, Javascript and Aiken

## Outputs

- [x] A documented project scaffold that demonstrates the end-to-end use of LambdaBuffers with Rust, JavaScript, Haskell and PlutusTx language ecosystems.

  - A project scaffold that demonstrates end-to-end use of LambdaBuffers with Rust can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/transactions/demo-rust)

  - A project scaffold that demonstrates end-to-end use of LambdaBuffers with JavaScript (TypeScript) can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/transactions/demo-typescript)

  - A project scaffold that demonstrates end-to-end use of LambdaBuffers with Haskell to create transactions can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/transactions/demo-haskell)

  - A project scaffold that demonstrates end-to-end use of LambdaBuffers with Haskell for onchain scripts using Plutarch can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/validation/demo-plutarch)

  - A project scaffold that demonstrates end-to-end use of LambdaBuffers with PlutusTx for onchain scripts can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/validation/demo-plutustx)

  - The LambdaBuffers schema used in all the aforementioned projects can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/api)

- [x] Demonstrate how to use LambdaBuffers to manage and exchange JSON-based configuration between different language ecosystems supported by LambdaBuffers.

  - The JSON-based configuration defined with LambdaBuffers is over [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/api/Demo/Config.lbf)

  - The Rust transaction building project uses such JSON-based configuration over [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/transactions/demo-rust/src/main.rs)

  - The JavaScript (TypeScript) transaction building project uses such JSON-based configuration in its testsuite [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/transactions/demo-typescript/src/tests/demo-test.ts)

  - The Haskell project uses such JSON-based configuration over [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/transactions/demo-haskell/src/Demo/Config.hs)

- [x] More importantly, demonstrates how to use LambdaBuffers to specify and exchange Plutus Datum between Plutus scripting languages and transaction-building frameworks.

  - A specification for Plutus Datum using LambdaBuffers is given [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/api/Demo/Plutus.lbf) where we see its usage in Plutus scripting languages and transaction-building frameworks in the following files:

    - With the PlutusTx Plutus scripting language, we see it deserializes the `EqDatum` in the `eqValidator` defined [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/validation/demo-plutustx/src/Demo/Validation.hs).

    - With the Plutarch Plutus scripting language, we see it deserializes the `EqDatum` in the `eqValidator` defined [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/validation/demo-plutarch/src/Demo/Validation.hs)

    - With Rust, we can see it uses an `EqDatum` to build a transaction over [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/transactions/demo-rust/src/lib.rs)

    - With TypeScript, we can see it uses an `EqDatum` to build a transaction over [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/transactions/demo-rust/src/lib.rs)

    - With Haskell, we can see it uses an `EqDatum` to build a transaction over [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/transactions/demo-haskell/src/Demo/Process.hs)

- [x] A fully tested and documented LambdaBuffers support for Rust, JavaScript, Haskell and PlutusTx language ecosystems.

  - A full example project with LambdaBuffers complete with automated tests for Rust can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/transactions/demo-rust)

  - A full example project with LambdaBuffers complete with automated tests for JavaScript (TypeScript) can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/transactions/demo-typescript)

  - A full example project with LambdaBuffers complete with automated tests for Haskell (using Rust's tx-village library) can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/transactions/demo-haskell) and [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/transactions/demo-tx-village)

  - A full example project with LambdaBuffers for creating validators with PlutusTx can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/validation/demo-plutustx)

- [x] A documented scaffold project that serves to demonstrate how to use LambdaBuffers with the newly supported languages for easy adoption.

  - Scaffold projects can be found for each language is provided above

  - The [`README.md`](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/README.md) serves as documentation for the scaffold projects which demonstrate how LambdaBuffers may be used with such languages

- [x] Final close-out report

  - This document is the close-out report

- [x] Final closeout video

  - Final closeout video can be found [here](https://youtu.be/0NyBALSAin0).

## Acceptance Criteria

- [x] A LambdaBuffers scaffold repository is made available that demonstrates a working end-to-end use of LambdaBuffers with Rust, JavaScript, Haskell and PlutusTx language ecosystems.

  - A LambdaBuffers scaffold repository that demonstrates a working end-to-end use of LambdaBuffers with Rust, JavaScript (TypeScript), Haskell, and PlutusTx is provided [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/transactions/demo-rust).

- [x] The scaffold uses LambdaBuffers to specify JSON configuration.

  - The JSON-based configuration defined with LambdaBuffers is over [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/api/Demo/Config.lbf)

- [x] The scaffold uses LambdaBuffers to specify Plutus Datum.

  - The scaffold uses LambdaBuffers to specify Plutus Datum [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/api/Demo/Plutus.lbf)

- [x] Final closeout video is publicly available

  - Final closeout video that is publicly available can be found [here](https://youtu.be/0NyBALSAin0).

## Evidence of Milestone Completion

- [x] The completed and reviewed LambdaBuffers scaffold project that uses LambdaBuffers to specify and exchange JSON based configuration between Rust, JavaScript, Haskell and PlutusTx language ecosystems.
  
  - [x] The completed and reviewed LambdaBuffers scaffold project that uses LambdaBuffers to specify and exchange JSON based configuration in Rust was implemented in PR [#12](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/pull/12)

  - [x] The completed and reviewed LambdaBuffers scaffold project that uses LambdaBuffers to specify and exchange JSON based configuration in JavaScript (TypeScript) was implemented in PR [#18](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/pull/18)

  - [x] The completed and reviewed LambdaBuffers scaffold project that uses LambdaBuffers to specify and exchange JSON based configuration in Haskell was implemented in PR [#27](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/pull/27)

  - [x] The completed and reviewed LambdaBuffers scaffold project that uses LambdaBuffers to specify and exchange JSON based configuration in PlutusTx was originally implemented in PR [#1](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/pull/1) and more recently updated in PR [#25](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/pull/25)

- [x] The completed and reviewed LambdaBuffers scaffold project that uses LambdaBuffers to specify and exchange Plutus Datums between Rust, JavaScript, Haskell and PlutusTx language ecosystems.
  
  - [x] The completed and reviewed LambdaBuffers scaffold project that uses LambdaBuffers to specify and exchange Plutus Datums configuration in Rust was implemented in PR [#12](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/pull/12)

  - [x] The completed and reviewed LambdaBuffers scaffold project that uses LambdaBuffers to specify and exchange Plutus Datums configuration in JavaScript (TypeScript) was implemented in PR [#18](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/pull/18)

  - [x] The completed and reviewed LambdaBuffers scaffold project that uses LambdaBuffers to specify and exchange Plutus Datums configuration in Haskell was implemented in PR [#27](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/pull/27)

  - [x] The completed and reviewed LambdaBuffers scaffold project that uses LambdaBuffers to specify and exchange Plutus Datums configuration in PlutusTx was originally implemented in PR [#1](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/pull/1) and more recently updated in PR [#25](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/pull/25)

- [x] We will record a video demonstrating the LambdaBuffers toolkit, how LambdaBuffers schemas are written and checked, and how code is generated from them in the newly supported programming languages.

  - A video demonstrating the LambdaBuffers toolkit, how LambdaBuffers schemas are written and checked, and how code is generated from them in the newly supported programming languages can be found [here](https://youtu.be/0NyBALSAin0).

- [x] We will publish a fully documented scaffold project that demonstrates how LambdaBuffers is used in Cardano dApp projects.

  - The fully documented scaffold project that demonstrates how LambdaBuffers is used in Cardano dApp projects is in the [lambda-buffers-for-cardano](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/) repository.

- [x] Link to final closeout video

  - Final closeout video can be found [here](https://youtu.be/0NyBALSAin0).

## References

- [Project Catalyst Final Milestone](https://milestones.projectcatalyst.io/projects/1000122/milestones/5)
