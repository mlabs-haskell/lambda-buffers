# Catalyst milestone 3: Testing and documentation

## Outputs

- [x] A test suite checking for correct mapping from schema data types to PlutusData encodings against a known-good corpus of such mappings (golden tests).
  - A dedicated [lbt-plutus](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-plutus) test suite was implemented for both [Haskell](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-plutus/lbt-plutus-haskell) and [Purescript](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-plutus/lbt-plutus-purescript) backends. They leverage both golden unit testing approach and randomized property based testing to assert the essential properties of the LambdaBuffers Plutus package:
    - `Plutus.V1.PlutusData` derivation tests
      - Golden unit tests: `forall (golden : Days.Day.*.pd.json): (toJson . toPlutusData . fromPlutusData . fromJson) golden == golden`
      - Property tests: `forall (x : Foo.*): (fromPlutusData . toPlutusData) x == x`
    - `Plutus.V1.PlutusData` instance tests
      - Golden unit tests: `forall (golden : *.pd.json): (toJson . toPlutusData . fromPlutusData . fromJson) golden == golden`

- [x] A test suite checking for roundtrip compatibility between codegened target environments.
  - A dedicated [lbt-plutus](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-plutus) test suite was implemented for both [Haskell](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-plutus/lbt-plutus-haskell) and [Purescript](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-plutus/lbt-plutus-purescript) backends.
  - A dedicated [lbt-prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-prelude) test suite was implemented for both [Haskell](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-prelude/lbt-prelude-haskell) and [Purescript](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-prelude/lbt-prelude-purescript) backends.
  - Both include golden unit tests that provide assurances that these backends implement the LambdaBuffers packages in a mutually compatible manner.
- [x] A modular and contract-based test suite architecture streamlining codegen testing compliance for any of the supported typeclasses.
  - A testing strategy was devised and [implemented](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites) where each supported backend must implement the `lbt` (ie. a LambdaBuffers test suite) for the corresponding LambdaBuffers package. A package is a collection of LambdaBuffers schemas and their associated runtime libraries.
  - LambdaBuffers Prelude
    - Schemas are available at [libs/lbf-prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/libs/lbf-prelude)
    - Haskell runtime library is in [runtimes/haskell/lbr-prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/runtimes/haskell/lbr-prelude)
    - Purescript runtime library is in [runtimes/purescript/lbr-prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/runtimes/purescript/lbr-prelude)
    - Haskell test suite is in [testsuites/lbt-prelude/lbt-prelude-haskell](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-prelude/lbt-prelude-haskell)
    - Purescript test suite is in [testsuites/lbt-prelude/lbt-prelude-purescript](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-prelude/lbt-prelude-purescript)
  - LambdaBuffers Plutus
    - Schemas are available at [libs/lbf-plutus](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/libs/lbf-plutus)
    - Haskell runtime library is in [runtimes/haskell/lbr-plutus](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/runtimes/haskell/lbr-plutus)
    - Purescript runtime library is in [runtimes/purescript/lbr-plutus](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/runtimes/purescript/lbr-plutus)
    - Haskell test suite is in [testsuites/lbt-plutus/lbt-plutus-haskell](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-plutus/lbt-plutus-haskell)
    - Purescript test suite is in [testsuites/lbt-plutus/lbt-plutus-purescript](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-plutus/lbt-plutus-purescript)
- [x] A document mapping the schema data types and typeclasses to their corresponding code-generated variants in the target environments.
  - [LambdaBuffers to Haskell](https://mlabs-haskell.github.io/lambda-buffers/haskell.html) documentation.
  - [LambdaBuffers to Purescript](https://mlabs-haskell.github.io/lambda-buffers/purescript.html) documentation.

## Acceptance Criteria

- [x] The test suites are passing for the Haskell+PlutusTx codegen module.
  - CI targets:
    - checks.x86_64-linux."check-lbt-prelude-haskell:test:tests"
    - checks.x86_64-linux."check-lbt-plutus-haskell:test:tests"
- [x] The test suites are passing for the Purescript+CTL codegen module.
  - CI targets:
    - checks.x86_64-linux."purescript:lbt-plutus:check"
    - checks.x86_64-linux."purescript:lbt-prelude:check"
- [x] The “Mappings” document is made available in the project repository.
  - [LambdaBuffers to Haskell](https://mlabs-haskell.github.io/lambda-buffers/haskell.html) documentation.
  - [LambdaBuffers to Purescript](https://mlabs-haskell.github.io/lambda-buffers/purescript.html) documentation.

## Evidence of Milestone Completion

- [x] The completed and reviewed test suite implementation for Haskell+PlutusTx codegen module is made available in the project repository.
  - LambdaBuffers Prelude
    - Schemas are available at [libs/lbf-prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/libs/lbf-prelude)
    - Haskell runtime library is in [runtimes/haskell/lbr-prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/runtimes/haskell/lbr-prelude)
    - Haskell test suite is in [testsuites/lbt-prelude/lbt-prelude-haskell](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-prelude/lbt-prelude-haskell)
  - LambdaBuffers Plutus
    - Schemas are available at [libs/lbf-plutus](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/libs/lbf-plutus)
    - Haskell runtime library is in [runtimes/haskell/lbr-plutus](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/runtimes/haskell/lbr-plutus)
    - Haskell test suite is in [testsuites/lbt-plutus/lbt-plutus-haskell](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-plutus/lbt-plutus-haskell)
- [x] The completed and reviewed test suite implementation for Purescript+CTL codegen module is made available in the project repository.
  - LambdaBuffers Prelude
    - Schemas are available at [libs/lbf-prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/libs/lbf-prelude)
    - Purescript runtime library is in [runtimes/purescript/lbr-prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/runtimes/purescript/lbr-prelude)
    - Purescript test suite is in [testsuites/lbt-prelude/lbt-prelude-purescript](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-prelude/lbt-prelude-purescript)
  - LambdaBuffers Plutus
    - Schemas are available at [libs/lbf-plutus](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/libs/lbf-plutus)
    - Purescript runtime library is in [runtimes/purescript/lbr-plutus](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/runtimes/purescript/lbr-plutus)
    - Purescript test suite is in [testsuites/lbt-plutus/lbt-plutus-purescript](https://github.com/mlabs-haskell/lambda-buffers/tree/e7813017bd1575fa9346ac4264deb68b50128b7c/testsuites/lbt-plutus/lbt-plutus-purescript)
- [x] The “Mappings” document is made available in the project repository.
  - [LambdaBuffers to Haskell](https://mlabs-haskell.github.io/lambda-buffers/haskell.html) documentation.
  - [LambdaBuffers to Purescript](https://mlabs-haskell.github.io/lambda-buffers/purescript.html) documentation.
- [x] Test case: Robust Test cases to catch edge conditions are added against a wide variety of schemas, and output in all codegen backends.
  - Implemented as part of `lbf` (LambdaBuffers Frontend) test suite.

## References

- [Catalyst project sheet](https://docs.google.com/spreadsheets/d/16dTxgGsxHvcMe5aCgFPDYEJKgX_VQiNAcwhp2RyA48o/edit#gid=1672366179)

## Documentation strategy

Each typeclass receives a Specification document that closely explains the required semantics for LB types (sum/products/records).

Each backend receives a User documentation that elaborates on how the generated code is used.

## Testing strategy

LambdaBuffers works with typeclasses, such as Eq, PlutusData and JSON. When a LB type has been declared with support for any of such typeclass, values of said types need to be handled in exactly the same manner as elaborated in the Specification for a given Typeclass in ALL support target language environments.

Concretely, if a type `Foo` has been declared with support for `JSON`, and `toJson` and `fromJson` methods have been generated for target languages, they have to be in correspondence.

```lbf
module Foo

import Prelude (Json)

sum Foo = Bar Integer | Baz Text

deriving Json Foo
```

In Haskell and Purescript values of `Foo` would be `Bar 1` and `Baz "baz"`, and their respective JSON mappings would be `{"constructor" : "Bar", "product" : [1]}` and `{"constructor" : "Baz", "product" : ["baz"]}`.

### Testing encoding typeclasses: `from . to` goldens

For each typeclass, we maintain a set of `golden' files of known good/bad that lift into the target language with`from` and write it back with `to` into a separate file. Then we provide an assertion that these files are semantically 'equal' (for example JSON spaces don't matter and such).

Example test:

- test/foo/Foo.lbf
- test/foo/json/bar.1.json
- test/foo/json/bar.2.json
- test/foo/json/baz.1.json
- test/foo/json/baz.2.json
- test/foo/json/haskell/bar.1.json
- test/foo/json/haskell/bar.2.json
- test/foo/json/haskell/baz.1.json
- test/foo/json/haskell/baz.2.json
- test/foo/json/purescript/bar.1.json
- test/foo/json/purescript/bar.2.json
- test/foo/json/purescript/baz.1.json
- test/foo/json/purescript/baz.2.json
- test/foo/plutusdata/bar.1.json
- test/foo/plutusdata/bar.2.json
- test/foo/plutusdata/baz.1.json
- test/foo/plutusdata/baz.2.json
- test/foo/plutusdata/haskell/bar.1.json
- test/foo/plutusdata/haskell/bar.2.json
- test/foo/plutusdata/haskell/baz.1.json
- test/foo/plutusdata/haskell/baz.2.json
- test/foo/plutusdata/purescript/bar.1.json
- test/foo/plutusdata/purescript/bar.2.json
- test/foo/plutusdata/purescript/baz.1.json
- test/foo/plutusdata/purescript/baz.2.json

### Testing equality typeclasses: symmetry and transitivity

- `sym :: (a :~: b) -> b :~: a`
- `trans :: (a :~: b) -> (b :~: c) -> a :~: c`

This could be done with goldens and randomized testing. However, `goldens` approach assumes a correct marshaling procedure while the `randomized` assumes generators. Perhaps having both makes sense.

## TODO (provide time estimates)

0. Define the version 1 of `Prelude` and `Plutus` LBF libraries which MUST be supported
1. Support the Json typeclass
   1. Codegen
   2. Runtime support
2. Support the Plutarch backend
3. Document typeclasses
   1. Eq
   2. PlutusData
   3. Json
4. Document backends
   1. Sum/Product/Record type construction and deconstruction (Haskell, Purescript, Plutarch)
   2. Eq/Json/PlutusData typeclass use (Haskell, Purescript, Plutarch minus Json)
5. Devise `goldens`
   1. Eq
   2. PlutusData
   3. Json
6. Implement the test suite
   1. Provide assertions
   2. Hook to the CI
