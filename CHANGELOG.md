# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Schemas

#### lbf-prelude
##### Purescript
##### Rust
##### Haskell
##### Typescript

#### lbf-plutus
##### Purescript
##### Rust
##### Haskell
##### Typescript
##### Plutarch
##### PlutusTx

### Tools

#### Frontend
#### Compiler
#### Compiler Proto API
#### Codegen
#### Codegen Proto API

### Runtimes

#### Rust lbr-prelude
#### Haskell lbr-prelude
#### Typescript lbr-prelude
#### Purescript lbr-prelude

#### Rust lbr-plutus
#### Haskell lbr-plutus
#### PlutusTx lbr-plutus
#### Plutarch lbr-plutus
#### Typescript lbr-plutus
#### Purescript lbr-plutus

## LambdaBuffers v2.0.0

Conway support, with new V3 types and updated dependencies.
V1 and V2 behaviour is unchanged, all serialization formats are backward compatible.

### Schemas

#### lbf-prelude

##### Purescript

Targets: [purescript-prelude 6.0.1](https://pursuit.purescript.org/packages/purescript-prelude), [purescript-js-bigints](https://pursuit.purescript.org/packages/purescript-js-bigints/), [purescript-maybe 6.0.0](https://pursuit.purescript.org/packages/purescript-maybe/), [purescript-either 6.1.0](https://pursuit.purescript.org/packages/purescript-either), [purescript-strings 6.0.1](https://pursuit.purescript.org/packages/purescript-strings), [ordered-collections 3.0.0](https://pursuit.purescript.org/packages/purescript-ordered-collections), and [purescript-aeson 2.0.0](https://github.com/mlabs-haskell/purescript-aeson)

##### Rust

Targets: [std 1.0.0](https://doc.rust-lang.org/std/), and [serde 1.0.188](https://serde.rs/)/[serde_json 1.0.107](https://docs.rs/serde_json/latest/serde_json/)

##### Haskell

Targets: [ghc 9.6.6](https://www.haskell.org/ghc/download_ghc_9_6_6.html) [base 4.18.2.1](https://hackage.haskell.org/package/base), [bytestring 0.11.5.2](https://hackage.haskell.org/package/bytestring), [text 2.0.2](https://hackage.haskell.org/package/text), and [aeson 2.2.3.0](https://hackage.haskell.org/package/aeson)

##### Typescript

Target: [prelude-typescript 1.0.1](https://github.com/mlabs-haskell/prelude-typescript/releases/tag/v1.0.1)

#### lbf-plutus

##### Purescript

Target: [cardano-transaction-library v9.3.x (unreleased)](https://github.com/Plutonomicon/cardano-transaction-lib/tree/b02718b7f8c04940dbf93dca7752d4fa6814b8d6)

##### Rust

Target [plutus-ledger-api 3.0.1](https://crates.io/crates/plutus-ledger-api/3.0.1)

##### Haskell

Target: [plutustx 1.36.0.0](https://github.com/IntersectMBO/plutus/releases/tag/1.36.0.0/plutus-tx)

##### Typescript

Target: [plutus-ledger-api-typescript 1.1.0](https://github.com/mlabs-haskell/plutus-ledger-api-typescript/releases/tag/v1.1.0)

##### Plutarch

Target: [plutarch 1.5.0](https://github.com/Plutonomicon/plutarch-plutus/tree/780d350f1985e89e3294861118f721d4141b2a6a)

##### PlutusTx

Target: [plutustx 1.36.0.0](https://github.com/IntersectMBO/plutus/releases/tag/1.36.0.0/plutus-tx)

### Tools

#### Frontend v.1.1.0.0
#### Compiler v.1.1.0.0
#### Compiler Proto API v.1.0.0
#### Codegen v.1.1.0.0
#### Codegen Proto API v.1.0.0
#### Utils v.1.1.0.0

### Runtimes

#### Rust lbr-prelude v.1.0.1
#### Haskell lbr-prelude v.1.1.0.0
#### Typescript lbr-prelude v.1.0.0
#### Purescript lbr-prelude v.1.0.0

#### Haskell lbr-plutus v.1.1.0.0
#### PlutusTx lbr-plutus v.1.0.0
#### Plutarch lbr-plutus v.1.0.0
#### Typescript lbr-plutus v.1.0.0
#### Purescript lbr-plutus v.1.0.0

## LambdaBuffers v.1.0.0

- Initial release

### Schemas

#### lbf-prelude v.1.0.0

##### Purescript

Targets: [purescript-prelude 6.0.1](https://pursuit.purescript.org/packages/purescript-prelude), [purescript-js-bigints](https://pursuit.purescript.org/packages/purescript-js-bigints/), [purescript-maybe 6.0.0](https://pursuit.purescript.org/packages/purescript-maybe/), [purescript-either 6.1.0](https://pursuit.purescript.org/packages/purescript-either), [purescript-strings 6.0.1](https://pursuit.purescript.org/packages/purescript-strings), [ordered-collections 3.0.0](https://pursuit.purescript.org/packages/purescript-ordered-collections), and [purescript-aeson 2.0.0](https://github.com/mlabs-haskell/purescript-aeson)

##### Rust

Targets: [std 1.0.0](https://doc.rust-lang.org/std/), and [serde 1.0.188](https://serde.rs/)/[serde_json 1.0.107](https://docs.rs/serde_json/latest/serde_json/)

##### Haskell

Targets: [base 4.18.1](https://hackage.haskell.org/package/base), [bytestring 0.11.5.2](https://hackage.haskell.org/package/bytestring), [text 2.0.2](https://hackage.haskell.org/package/text), and [aeson 2.2.1.0](https://hackage.haskell.org/package/aeson)

##### Typescript

Target: [prelude-typescript 1.0.1](https://github.com/mlabs-haskell/prelude-typescript/releases/tag/v1.0.1)

#### lbf-plutus v.1.0.0

##### Purescript

Target: [cardano-transaction-library v7.0.0](https://github.com/Plutonomicon/cardano-transaction-lib/releases/tag/v7.0.0)

##### Rust

Target [plutus-ledger-api 1.0.0](https://crates.io/crates/plutus-ledger-api/1.0.0)

##### Haskell

Target: [plutustx 1.20.0.0](https://github.com/IntersectMBO/plutus/tree/1.20.0.0/plutus-tx)

##### Typescript

Target: [plutus-ledger-api-typescript 1.0.0](https://github.com/mlabs-haskell/plutus-ledger-api-typescript/releases/tag/v1.0.0)

##### Plutarch

Target: [plutarch 1.5.0](https://github.com/Plutonomicon/plutarch-plutus/tree/780d350f1985e89e3294861118f721d4141b2a6a)

##### PlutusTx

Target: [plutustx 1.20.0.0](https://github.com/IntersectMBO/plutus/tree/1.20.0.0/plutus-tx)

### Tools

#### Frontend v.1.1.0.0
#### Compiler v.1.1.0.0
#### Compiler Proto API v.1.0.0
#### Codegen v.1.1.0.0
#### Codegen Proto API v.1.0.0
#### Utils v.1.1.0.0

### Runtimes

#### Rust lbr-prelude v.0.1.3
#### Haskell lbr-prelude v.1.1.0.0
#### Typescript lbr-prelude v.1.0.0
#### Purescript lbr-prelude v.1.0.0

#### Haskell lbr-plutus v.1.1.0.0
#### PlutusTx lbr-plutus v.1.0.0
#### Plutarch lbr-plutus v.1.0.0
#### Typescript lbr-plutus v.1.0.0
#### Purescript lbr-plutus v.1.0.0
