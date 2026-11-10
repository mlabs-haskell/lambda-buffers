# Backend support for partially supported types

Most `lbf-prelude` types are supported by every backend. Three are not. This page lists them,
where each one is supported and why the rest are not. All other prelude types work in every
backend.

## Compatibility table

| Prelude type  | Haskell | PlutusTx | Plutarch | PureScript | Rust | TypeScript |
|---------------|:-------:|:--------:|:--------:|:----------:|:----:|:----------:|
| `G1Element`   | ✅ HBLS `G1Point` | ✅ `BuiltinBLS12_381_G1_Element` | ✅ `PBuiltinBLS12_381_G1_Element` | ❌ | ❌ | ❌ |
| `G2Element`   | ✅ HBLS `G2Point` | ✅ `BuiltinBLS12_381_G2_Element` | ✅ `PBuiltinBLS12_381_G2_Element` | ❌ | ❌ | ❌ |
| `BuiltinValue`| ❌ | ✅ `BuiltinValue` | ✅ `PBuiltinValue` | ❌ | ❌ | ❌ |

✅ maps to the native type shown. ❌ maps to `Unsupported`, an uninhabited placeholder type in
that backend's runtime: a schema that uses the type still generates and compiles, but no
value of it can be constructed.

Class instances:

- `G1Element` and `G2Element` have `Eq`, and no `Json`. BLS points are not meaningful
  transaction or validator data, and HBLS has no JSON encoding for them.
- `BuiltinValue` (and `Array a`, which every backend supports) has neither `Eq` nor `Json`.
  An instance declared in the schema must hold in every backend that supports the type, and
  Plutarch provides no `Eq` for `PArray` (any implementation would be prohibitively costly
  on-chain) or for `PBuiltinValue` in the version targeted here.

## Why these gaps exist

- **BLS12-381 points** (`G1Element`, `G2Element`) must agree exactly with the Plutus
  operations on them, which are bindings to the `blst` C library. Haskell gets this through
  [HBLS](https://github.com/mlabs-haskell/hbls); PureScript, Rust and TypeScript have no
  equivalent binding in their LambdaBuffers runtimes.
- **`BuiltinValue`** has no Haskell-level constructor or destructor: `plutus-core` exposes
  conversions to and from `Value` only inside Plutus. A Haskell type you can neither build
  nor inspect would be no use, so it stays `Unsupported` there. The off-chain backends are in
  the same position.
- **The root cause** is that `lbf-prelude` holds two kinds of type: primitives every backend
  can be expected to support, and Plutus builtin types that only backends sharing Plutus's
  representation can support. Before the van Rossem builtins the two sets coincided.

In practice the impact is small: these types are only useful in on-chain code, where the
Plutarch and PlutusTx backends support them.

## Direction

We intend to find the design that gives the best usability for these types in the medium
term. The leading option is to split `lbf-prelude` into a schema every backend must support
and a schema for Plutus builtin types, so a backend opts in to the second instead of mapping
its types to `Unsupported`. That change touches the test orchestration and the Nix build, so
it is planned as its own piece of work. See the
[v2.1.0 changelog](https://github.com/mlabs-haskell/lambda-buffers/blob/main/CHANGELOG.md#lambdabuffers-v210)
for the full background.
