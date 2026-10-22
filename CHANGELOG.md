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
Plutus ledger language version. No breaking LambdaBuffers API or serialization changes
were required: existing schemas and serialization formats remain backward compatible.
The prelude is extended with opaque types for the new builtins, implemented only for
the Haskell-family backends; the other backends map them to an uninhabited
`Unsupported` placeholder.

Notable dependency and Nix environment changes:

- All Haskell Plutus projects are pinned to a uniform
  `plutus-core`/`plutus-ledger-api`/`plutus-tx`/`plutus-tx-plugin` `==1.65.0.0`
  (the newest version supported by Plutarch; PV11-capable, which requires `>=1.63`),
  resolved from Cardano Haskell Packages (CHaP) at rev `f77658b` (2026-07).
- The GHC compiler was bumped from 9.6.7 to **9.12.1**. This is required: Plutarch 1.14
  uses `TypeAbstractions` (GHC `>=9.8`), while `plutus-tx-plugin` 1.65 is only buildable
  on GHC 9.6.x or 9.12.x — GHC 9.12 is the only version that satisfies both.
- The `flake-lang` input is pinned and its `cardano-haskell-packages` (CHaP) input is
  overridden to the 2026-07 rev for plutus 1.65. haskell.nix and its bundled hackage.nix
  are left at flake-lang's defaults (whose GHC 9.12.1 is cache-built), so the Hackage
  index-state stays at `2025-09-27`. The few packages that only gained GHC 9.12 support
  later than that — `proto-lens` 0.7.1.7 — are injected per-project as haskell.nix
  extraHackage source dependencies (`settings.haskell.proto-lens-deps`) rather than via a
  hackage input override, which keeps the flake evaluable by Hercules CI in pure mode.
- Plutarch bumped to [1.14.0](https://github.com/Plutonomicon/plutarch-plutus/tree/c3b4771901b2bc5ba0c1e1fed66de3c0f5523069)
  (`ClosedTerm` removed, `PMap`/`PValue` replaced by `PSortedMap`/`PSortedValue`;
  the lbr-plutarch runtime was migrated accordingly).
- BLS support in the Haskell backends comes from
  [HBLS](https://github.com/mlabs-haskell/hbls) pinned at `aa99224`, which builds its own
  vendored `blst` C and assembly sources. `settings.haskell.hbls-src` supplies it to every
  project that needs it.
- Purescript projects bumped to [purescript-cardano-package-set v4.1.0](https://github.com/mlabs-haskell/purescript-cardano-package-set/tree/v4.1.0)
  and cardano-transaction-lib pinned to the [van Rossem compatibility PR #1687](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1687)
  (to be re-pinned once merged).
- Rust testsuite bumped to [plutus-ledger-api 3.1.0](https://crates.io/crates/plutus-ledger-api/3.1.0).
  Generated Rust crates now build against this repo's runtime sources
  (`lbr-prelude`, and a `plutus-ledger-api` 3.1.0 source tree whose `lbr-prelude`
  dependency is rewritten to a path) instead of crates.io, so a single `lbr-prelude`
  is present in the dependency graph.
- The shared dev shell now includes `nix-prefetch-git` (required by `spago2nix generate`).

### Schemas

#### Prelude type support notes

The various `Unsupported` wrappers are due to an architectural shortcoming that we did
not have the resources to address: LambdaBuffers as currently implemented does not
distinguish between *universe types* and *Plutus types* in the right way. At present,
the `prelude` schema muddles these two things together: `G1Element`, for example, is a
Plutus primitive which must be represented as an LB `opaque`, but which cannot easily be
ported to other languages because conformance with the Plutus operations requires exact
conformance with Haskell bindings to the underlying C library. The purpose of `prelude`
is, AFAICT, to hold types which are primitive and which we can reasonably expect any
backend to support. That was reasonable in earlier versions of Plutus, which contained
only primitives which could be expected to exist in any language worth writing a backend
for. The BLS primitives break that assumption.

The proper solution requires a redesign of core LB functionality which cannot be done
within our budget. Specifically, we would need to bifurcate `prelude` into a *true*
"every backend **must** support this" LB schema and a "these are only for backends which
can represent `DefaultUni` types the same way Haskell does" schema. We did not have the
time/budget to do that here, but it would obviate the need for all of these
`Unsupported`s. (We cannot just add a new schema due to the way the tests and build
system are orchestrated; it requires significant revisions to the entire testing setup
and the Nix infrastructure.)

Some additional notes:

- We are also limited by a design constraint that, if a type is supported in a backend,
  all of its declared instances (instances are declared in the LB schema that introduces
  the type) must be supported as well. (This I am fairly sure of — I wrote the original
  constraint solver and it definitely works at a backend-agnostic level, and things would
  break horribly if we lied about type classes.) Consequently, inability to support a
  single instance precludes implementation of the type class. This is unfortunate and
  requires a rethink of the compiler pipeline, which, again, is just too much to do
  within our budget. (The one safe exception to this is the case where the type class
  itself makes no sense in the target language, e.g. JSON in Plutarch or PlutusTx, but
  that doesn't materially help us here.)
- Plutarch does not provide an `Eq` instance for `Array` on purpose because the best
  possible implementation is comically inefficient. This cannot and should not be fixed
  upstream, so we are left with no choice but to omit that instance, because Plutarch
  *does* support `Eq` in general.
- Similarly, Plutarch does not support `Eq` for `BuiltinValue` in the version we target
  here, which rules that instance out.
- The lack of support for `BuiltinValue` in Haskell may seem confusing, but follows from
  the lack of exposed structure for that type in `plutus-core` where it is defined. There
  are *Plutus-level* functions for converting between `BuiltinValue` and `Value` but (as
  far as we can tell) no Haskell-level functions. We could write them if we could construct or
  disassemble `BuiltinValue`, but Plutus does not provide the means to do so, and it
  seems safer to leave it unsupported than to add support for a type that you cannot do
  anything meaningful with.

The actual significance of these limitations for a user is fairly limited — since you
can't do anything useful with the unsupported types, you probably won't be that annoyed
that we don't let you try.

##### Note on Test Coverage

LambdaBuffers maintains a per-backend translation table mapping each schema type to a
concrete type in the target language. During code generation, every entry in the prelude
schema is emitted into a generated prelude module as an import plus a type alias, and that
module is built as an ordinary library in the target language. Because the generated
libraries are declared as dependencies of the integration test suites, the Nix build
compiles them before the tests run; a typo in a package, module, or type name, or a
missing dependency, therefore fails the build for that backend rather than surfacing
later. This gives compile-time coverage of all prelude mappings across Haskell, Plutarch,
PlutusTx, Rust, TypeScript, and PureScript, including the new BLS, array, and
builtin-value types.

The check is structural. It confirms that each mapped type resolves and that the
generated alias is well-formed for its target language, including type-parameter arity
and import paths. Whenever those packages are built, the same build graph that compiles
the runtimes and test suites compiles the generated prelude modules, so a broken entry
appears as a build failure.

#### lbf-prelude

New opaque types: `G1Element`, `G2Element`, `Array a`, and `BuiltinValue`.
`G1Element` and `G2Element` have `Eq` instances. They deliberately do not have
`Json` instances: BLS points are not meaningful transaction or validator data, and
the Haskell runtime does not implement JSON serialization for HBLS points.

##### Purescript

`Array a` maps to `Prim.Array`. `G1Element`, `G2Element`, and `BuiltinValue` map to
`Unsupported`, an uninhabited placeholder: BLS and `BuiltinValue` are not implemented
for this backend.

##### Rust

`Array a` maps to `Vec`. `G1Element`, `G2Element`, and `BuiltinValue` map to
`Unsupported`, an empty enum and therefore uninhabited.

##### Haskell

`G1Element` and `G2Element` map to HBLS `G1Point` and `G2Point`; `Array a` maps to
`Data.Vector.Vector`; `BuiltinValue` maps to `Unsupported`.

##### Typescript

`Array a` maps to `List`. `G1Element`, `G2Element`, and `BuiltinValue` map to
`Unsupported`, defined as `never`.

##### PlutusTx

`G1Element` and `G2Element` map to `BuiltinBLS12_381_G1_Element` and
`BuiltinBLS12_381_G2_Element`; `Array a` maps to `BuiltinArray`; `BuiltinValue` maps to
`BuiltinValue`.

##### Plutarch

`G1Element` and `G2Element` map to `PBuiltinBLS12_381_G1_Element` and
`PBuiltinBLS12_381_G2_Element`; `Array a` maps to `PArray`; `BuiltinValue` maps to
`PBuiltinValue`.

#### lbf-plutus

No schema changes.

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

### Tools

#### Codegen v.1.1.0.0

Updated the prelude type mappings for every backend to cover the new prelude types.
No compiler or frontend changes.

### Runtimes

#### Rust lbr-prelude v.1.0.2

Added `Unsupported`, an empty enum used for prelude types the Rust backend does not
implement.

#### Haskell lbr-prelude v.1.1.0.0

Added `Unsupported`, an empty data type used for prelude types the backend does not
implement.

#### Typescript lbr-prelude v.1.0.0

Added `Unsupported` (`never`) and its corresponding symbol, used for prelude types the
Typescript backend does not implement.

#### Purescript lbr-prelude v.1.0.0

Added `Unsupported`, an empty data type used for prelude types the Purescript backend
does not implement.

#### Haskell lbr-plutustx v.1.1.0.0

Added an `Array` newtype and an `hbls` dependency; `Set` gained a phantom role
annotation.

#### Plutarch lbr-plutus v.1.1.0.0

Migrated to Plutarch 1.14.0: `ClosedTerm` usages replaced with polymorphic `Term`
signatures, `PMap` now aliases `PSortedMap`, and `PValue` now aliases `PSortedValue`.

### Build and test infrastructure

- The PlutusTx testsuite compiled ~70 Plinth splices in a single GHC process, peaking at
  roughly 20 GB RSS because Plinth retains per-splice working memory until the process
  exits. The splices are now split across three internal libraries (`splices-a`,
  `splices-b`, `splices-c`), each built by its own GHC process and chained by
  dependencies, reducing peak RSS to roughly 9.4 GB; the test suite is unchanged and
  still passes.
- Removed `fooECompiled`, `fooFIntCompiled`, and `fooGIntCompiled` from the PlutusTx
  testsuite: `fooE` exceeded 62 GB when compiled on its own, and the other two hit
  Plinth's "mutually recursive datatypes" limitation.
- Plutarch's source is copied with `-Werror` stripped from its cabal files, since
  Plutarch 1.14 is only warning-clean on its tested GHC.
- `freer-simple` is patched for GHC 9.12 by adding the missing `Monad b` constraint to
  its `MonadBase` instance.
- Warning flags were updated for GHC 9.12 (`-Wno-missing-role-annotations`,
  `-Wno-missing-poly-kind-signatures`, `-Wno-unused-packages`).

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
