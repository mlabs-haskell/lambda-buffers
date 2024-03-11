# Catalyst milestone 1: Rust support

## Outputs

- [x] A LambdaBuffers code generation module that outputs type definitions and derived implementations in the Rust programming language given a LambdaBuffers schema.
  - The module implementation is in [lambda-buffers-codegen/src/LambdaBuffers/Codegen/Rust](https://github.com/mlabs-haskell/lambda-buffers/tree/c83cdf9b881a95b8607821027c3551ecd56c9447/lambda-buffers-codegen/src/LambdaBuffers/Codegen/Rust).
- [x] A Rust library that implements the LambdaBuffers Prelude runtime. This module would include standardised JSON encoding and equality implementations for all declared type class instances in the schema.
  - A Prelude library for Rust was implemented together with a separate library for Json trait instance derive macros. These can be found here:
    - [runtimes/rust/lbr-prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/c83cdf9b881a95b8607821027c3551ecd56c9447/runtimes/rust/lbr-prelude)
    - [runtimes/rust/lbr-prelude-derive](https://github.com/mlabs-haskell/lambda-buffers/tree/c83cdf9b881a95b8607821027c3551ecd56c9447/runtimes/rust/lbr-prelude-derive)
- [x] A Rust test suite that assures the manually implemented and automatically generated implementations are consistent with the predefined LambdaBuffers Prelude golden data set of JSON files and perform correct implementation derivation.
  - A test suite was implemented with automatically generated Rust types and trait implementations for Prelude types, it can be found here: [testsuites/lbt-prelude/lbt-prelude-rust](https://github.com/mlabs-haskell/lambda-buffers/tree/c83cdf9b881a95b8607821027c3551ecd56c9447/testsuites/lbt-prelude/lbt-prelude-rust)
- [x] A Rust library that implements the LambdaBuffers Plutus runtime. This module would include standardised PlutusData encoding implementations for all declared type class instances in the Plutus schema.
  - A standalone library was implemented (still in active development) with Plutus ledger types. LambdaBuffers runtime functionality is included in this library: [plutus-ledger-api-rust](https://github.com/mlabs-haskell/plutus-ledger-api-rust/tree/23eb5df1be03e5983865867f74a2933b7063414d).
- [x] A Rust test suite that assures the manually implemented and automatically generated implementations are consistent with the predefined LambdaBuffers Plutus golden data set of PlutusData encoded files and perform correct implementation derivation.
  - A test suite was implemented with automatically generated Rust types and trait implementations for Plutus types, it can be found here: [testsuites/lbt-plutus/lbt-plutus-rust](https://github.com/mlabs-haskell/lambda-buffers/tree/c83cdf9b881a95b8607821027c3551ecd56c9447/testsuites/lbt-plutus/lbt-plutus-rust)
- [x] Nix devops modules (Nix API) for streamlining the LambdaBuffers code generation pipeline to Rust.
  - New flake modules were implemented to easily generate Rust crates from LambdaBuffers:
    - [lbf-rust](https://github.com/mlabs-haskell/lambda-buffers/blob/c83cdf9b881a95b8607821027c3551ecd56c9447/extras/lbf-nix/lbf-rust.nix)
    - [lbf-prelude-rust](https://github.com/mlabs-haskell/lambda-buffers/blob/c83cdf9b881a95b8607821027c3551ecd56c9447/extras/lbf-nix/lbf-prelude-rust.nix)
    - [lbf-plutus-rust](https://github.com/mlabs-haskell/lambda-buffers/blob/c83cdf9b881a95b8607821027c3551ecd56c9447/extras/lbf-nix/lbf-plutus-rust.nix)
- [x] Documentation on LambdaBuffers usage patterns for Rust.
  - [A new page was created for the Rust use case](https://mlabs-haskell.github.io/lambda-buffers/rust.html)

## Acceptance Criteria

- [x] LambdaBuffers schemas that are based on the LambdaBuffers Prelude module can be used in Rust projects to specify application types.
  - Test libraries for Prelude demonstrate how Rust code for LambdaBuffers Prelude is generated to Rust and used in a library: [testsuites/lbt-prelude/lbt-prelude-rust](https://github.com/mlabs-haskell/lambda-buffers/tree/c83cdf9b881a95b8607821027c3551ecd56c9447/testsuites/lbt-prelude/lbt-prelude-rust)
- [x] LambdaBuffers schemas that are based on the LambdaBuffers Plutus module can be used in Rust projects to specify application types.
  - Test libraries for Plutus demonstrate how Rust code for LambdaBuffers Plutus is generated to Rust and used in a library: [testsuites/lbt-plutus/lbt-plutus-rust](https://github.com/mlabs-haskell/lambda-buffers/tree/c83cdf9b881a95b8607821027c3551ecd56c9447/testsuites/lbt-plutus/lbt-plutus-rust)
- [x] The documentation and devops tooling is available to facilitate easy adoption.
  - Similarly to other languages supported by LambdaBuffers, a Rust flake is implemented. The testing libraries also serve as an example, to understand how to use these Nix utilities:
    [testsuites/lbt-plutus/lbt-plutus-rust/build.nix](https://github.com/mlabs-haskell/lambda-buffers/tree/c83cdf9b881a95b8607821027c3551ecd56c9447/testsuites/lbt-plutus/lbt-plutus-rust/build.nix)

## Evidence of Milestone Completion

- [x] The completed and reviewed LambdaBuffers Prelude runtime library is available for the Rust programming language.
  - [runtimes/rust/lbr-prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/c83cdf9b881a95b8607821027c3551ecd56c9447/runtimes/rust/lbr-prelude)
  - [runtimes/rust/lbr-prelude-derive](https://github.com/mlabs-haskell/lambda-buffers/tree/c83cdf9b881a95b8607821027c3551ecd56c9447/runtimes/rust/lbr-prelude-derive)
- [x] The completed and reviewed LambdaBuffers Plutus runtime library is available for the Rust programming language.
  - [plutus-ledger-api-rust](https://github.com/mlabs-haskell/plutus-ledger-api-rust/tree/23eb5df1be03e5983865867f74a2933b7063414d).
- [x] The completed and reviewed LambdaBuffers Prelude test suite is available and is passing in CI for the Rust programming language.
  - [testsuites/lbt-prelude/lbt-prelude-rust](https://github.com/mlabs-haskell/lambda-buffers/tree/c83cdf9b881a95b8607821027c3551ecd56c9447/testsuites/lbt-prelude/lbt-prelude-rust)
- [x] The completed and reviewed LambdaBuffers Plutus test suite is available and is passing in CI for the Rust programming language.
  - [testsuites/lbt-plutus/lbt-plutus-rust](https://github.com/mlabs-haskell/lambda-buffers/tree/c83cdf9b881a95b8607821027c3551ecd56c9447/testsuites/lbt-plutus/lbt-plutus-rust)
- [x] The completed and reviewed Nix API for LambdaBuffers Rust support is available.
  - [lbf-rust](https://github.com/mlabs-haskell/lambda-buffers/blob/c83cdf9b881a95b8607821027c3551ecd56c9447/extras/lbf-nix/lbf-rust.nix)
  - [lbf-prelude-rust](https://github.com/mlabs-haskell/lambda-buffers/blob/c83cdf9b881a95b8607821027c3551ecd56c9447/extras/lbf-nix/lbf-prelude-rust.nix)
  - [lbf-plutus-rust](https://github.com/mlabs-haskell/lambda-buffers/blob/c83cdf9b881a95b8607821027c3551ecd56c9447/extras/lbf-nix/lbf-plutus-rust.nix)
- [x] The completed and reviewed LambdaBuffers for Rust documentation is available.
  - [Rust documentation](https://mlabs-haskell.github.io/lambda-buffers/rust.html)

## Demo files

- Demo project: [lambda-buffers-for-cardano](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/transactions/demo-rust)
