# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- ## [Unreleased]

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

#### Purescript lbr-plutus -->

## LambdaBuffers v2.1.0

Cardano van Rossem hard fork (intra-era Conway, major protocol version 11) support,
with all Cardano-related dependencies updated to current PV11-compatible versions.

The van Rossem hard fork introduces new Plutus builtins (CIP-109 `expModInteger`,
CIP-132 `dropList`, CIP-133 BLS12-381 multi-scalar multiplication, CIP-138 arrays,
CIP-153 `Value` builtins) and updated cost models, without a new ledger era or a new
Plutus ledger language version. No LambdaBuffers API or serialization changes were
required; all serialization formats remain backward compatible.

Notable dependency and Nix environment changes:

- Haskell Plutus projects now resolve against Cardano Haskell Packages (CHaP) at
  index-state `2026-07-09T17:40:37Z`, with all projects pinned to a uniform
  `plutus-core`/`plutus-ledger-api`/`plutus-tx`/`plutus-tx-plugin` `==1.65.0.0`
  (the newest version supported by Plutarch; PV11-capable, which requires `>=1.63`).
- The GHC compiler was bumped from 9.6.7 to **9.12.1**. This is required: Plutarch 1.14
  uses `TypeAbstractions` (GHC `>=9.8`), while `plutus-tx-plugin` 1.65 is only buildable
  on GHC 9.6.x or 9.12.x — GHC 9.12 is the only version that satisfies both.
- The `flake-lang` input now overrides its `cardano-haskell-packages` and
  `haskell-nix/hackage` inputs so haskell.nix can resolve the 2026 package sets.
- Plutarch bumped to [1.14.0](https://github.com/Plutonomicon/plutarch-plutus/tree/c3b4771901b2bc5ba0c1e1fed66de3c0f5523069)
  (`ClosedTerm` removed, `PMap`/`PValue` replaced by `PSortedMap`/`PSortedValue`;
  the lbr-plutarch runtime was migrated accordingly).
- Purescript projects bumped to [purescript-cardano-package-set v4.1.0](https://github.com/mlabs-haskell/purescript-cardano-package-set/tree/v4.1.0)
  and cardano-transaction-lib pinned to the [van Rossem compatibility PR #1687](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1687)
  (to be re-pinned once merged).
- Rust testsuite bumped to [plutus-ledger-api 3.1.0](https://crates.io/crates/plutus-ledger-api/3.1.0).
- The shared dev shell now includes `nix-prefetch-git` (required by `spago2nix generate`).

### Schemas

#### lbf-plutus

##### Purescript

Target: [cardano-transaction-lib van Rossem HF compatibility (PR #1687, unreleased)](https://github.com/Plutonomicon/cardano-transaction-lib/tree/eedf26dda9cb19088975981af8e2311b054c2f07)

##### Rust

Target: [plutus-ledger-api 3.1.0](https://crates.io/crates/plutus-ledger-api/3.1.0)

##### Haskell

Target: [plutus-tx 1.65.0.0](https://github.com/IntersectMBO/plutus/releases/tag/1.65.0.0) on [ghc 9.12.1](https://www.haskell.org/ghc/download_ghc_9_12_1.html)

##### Typescript

Target: [plutus-ledger-api-typescript 1.2.2](https://github.com/mlabs-haskell/plutus-ledger-api-typescript/releases/tag/v1.2.2) (unchanged)

##### Plutarch

Target: [plutarch 1.14.0](https://github.com/Plutonomicon/plutarch-plutus/tree/c3b4771901b2bc5ba0c1e1fed66de3c0f5523069)

##### PlutusTx

Target: [plutus-tx 1.65.0.0](https://github.com/IntersectMBO/plutus/releases/tag/1.65.0.0)

### Runtimes

#### Plutarch lbr-plutus v.1.1.0

Migrated to Plutarch 1.14.0: `ClosedTerm` usages replaced with polymorphic `Term`
signatures, `PMap` now aliases `PSortedMap`, and `PValue` now aliases `PSortedValue`.

## LambdaBuffers v2.0.0

Conway support, with new V3 types and updated dependencies.

V1 and V2 behaviour is unchanged, all serialization formats are backward compatible.
Migrating LambdaBuffers from from v1.0.0 does not require any changes on the
API level, however all libraries have been updated (CTL, plutus-ledger-api-rust, plutus-tx, etc.),
most of them containing breaking changes. For migrations guides, please refer to the
documentation of these libraries. You can find the library versions below:

### Schemas

#### lbf-prelude

##### Purescript

Targets: [purescript-prelude 6.0.1](https://pursuit.purescript.org/packages/purescript-prelude), [purescript-js-bigints](https://pursuit.purescript.org/packages/purescript-js-bigints/), [purescript-maybe 6.0.0](https://pursuit.purescript.org/packages/purescript-maybe/), [purescript-either 6.1.0](https://pursuit.purescript.org/packages/purescript-either), [purescript-strings 6.0.1](https://pursuit.purescript.org/packages/purescript-strings), [ordered-collections 3.0.0](https://pursuit.purescript.org/packages/purescript-ordered-collections), and [purescript-aeson 2.0.0](https://github.com/mlabs-haskell/purescript-aeson)

##### Rust

Targets: [std 1.0.0](https://doc.rust-lang.org/std/), and [serde 1.0.188](https://serde.rs/)/[serde_json 1.0.107](https://docs.rs/serde_json/latest/serde_json/)

##### Haskell

Targets: [ghc 9.6.6](https://www.haskell.org/ghc/download_ghc_9_6_6.html) [base 4.18.2.1](https://hackage.haskell.org/package/base), [bytestring 0.11.5.2](https://hackage.haskell.org/package/bytestring), [text 2.0.2](https://hackage.haskell.org/package/text), and [aeson 2.2.3.0](https://hackage.haskell.org/package/aeson)

##### Typescript

Target: [prelude-typescript 1.0.2](https://github.com/mlabs-haskell/prelude-typescript/releases/tag/v1.0.2)

#### lbf-plutus

##### Purescript

Target: [cardano-transaction-library v9.3.x (unreleased)](https://github.com/Plutonomicon/cardano-transaction-lib/tree/b02718b7f8c04940dbf93dca7752d4fa6814b8d6)

##### Rust

Target [plutus-ledger-api 3.0.1](https://crates.io/crates/plutus-ledger-api/3.0.1)

##### Haskell

Target: [plutustx 1.36.0.0](https://github.com/IntersectMBO/plutus/releases/tag/1.36.0.0/plutus-tx)

##### Typescript

Target: [plutus-ledger-api-typescript 1.2.1](https://github.com/mlabs-haskell/plutus-ledger-api-typescript/releases/tag/v1.2.1)

##### Plutarch

Target: [plutarch 1.5.0](https://github.com/Plutonomicon/plutarch-plutus/tree/e9e9df286768440733890b1260ad569a2f882890)

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
