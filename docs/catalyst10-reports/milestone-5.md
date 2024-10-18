# Catalyst milestone 5 (FINAL): Project scaffold for Rust, Javascript and Aiken

## Outputs

- [x] A documented project scaffold that demonstrates the end-to-end use of LambdaBuffers with Rust, JavaScript, Haskell and PlutusTx language ecosystems.

  - A project scaffold that demonstrates end-to-end use of LambdaBuffers with Rust can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/transactions/demo-rust)

  - A project scaffold that demonstrates end-to-end use of LambdaBuffers with JavaScript (TypeScript) can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/transactions/demo-typescript)

  - TODO(jaredponn) October 18: this depends on [#27](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/pull/27) being merged
    A project scaffold that demonstrates end-to-end use of LambdaBuffers with Haskell to create transactions can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/transactions/demo-haskell)

  - A project scaffold that demonstrates end-to-end use of LambdaBuffers with Haskell for onchain scripts using Plutarch can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/validation/demo-plutarch)

  - A project scaffold that demonstrates end-to-end use of LambdaBuffers with PlutusTx for onchain scripts can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/validation/demo-plutustx)

  - The LambdaBuffers schema used in all the aforementioned projects can be found [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/api)

- [x] Demonstrate how to use LambdaBuffers to manage and exchange JSON-based configuration between different language ecosystems supported by LambdaBuffers.

  - The JSON-based configuration defined with LambdaBuffers is over [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/api/Demo/Config.lbf)

  - The Rust transaction building project uses such JSON-based configuration over [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/transactions/demo-rust/src/main.rs)

  - The JavaScript (TypeScript) transaction building project uses such JSON-based configuration in its testsuite [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/transactions/demo-typescript/src/tests/demo-test.ts)

  - TODO(jaredponn) October 18: this depends on [#27](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/pull/27) being merged
    Haskell project uses such JSON-based configuration over [here](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/blob/main/transactions/demo-rust/src/main.rs)

- [x] More importantly, demonstrates how to use LambdaBuffers to specify and exchange Plutus Datum between Plutus scripting languages and transaction-building frameworks.

  - AH TODO

- [ ] A fully tested and documented LambdaBuffers support for Rust, JavaScript, Haskell and PlutusTx language ecosystems.
- [ ] A documented scaffold project that serves to demonstrate how to use LambdaBuffer with the newly supported languages for easy adoption.
- [ ] Final close-out report
- [ ] Final closeout video

## Acceptance Criteria

- [ ] A LambdaBuffers scaffold repository is made available that demonstrates a working end-to-end use of LambdaBuffers with Rust, JavaScript, Haskell and PlutusTx language ecosystems.
- [ ] The scaffold uses LambdaBuffers to specify JSON configuration.
- [ ] The scaffold uses LambdaBuffers to specify Plutus Datum.
- [ ] Final closeout report is publicly available
- [ ] Final closeout video is publicly available

## Evidence of Milestone Completion

- [ ] The completed and reviewed LambdaBuffers scaffold project that uses LambdaBuffers to specify and exchange JSON based configuration between Rust, JavaScript, Haskell and PlutusTx language ecosystems.
- [ ] The completed and reviewed LambdaBuffers scaffold project that uses LambdaBuffers to specify and exchange Plutus Datums between Rust, JavaScript, Haskell and PlutusTx language ecosystems.
- [ ] We will record a video demonstrating the LambdaBuffers toolkit, how LambdaBuffers schemas are written and checked, and how code is generated from them in the newly supported programming languages.
- [ ] We will publish a fully documented scaffold project that demonstrates how LambdaBuffers is used in Cardano dApp projects.
- [ ] Link to final closeout report
- [ ] Link to final closeout video

## References

- [Project Catalyst Final Milestone](https://milestones.projectcatalyst.io/projects/1000122/milestones/5)
