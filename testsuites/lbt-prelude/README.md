# lbt-prelude

Testsuite for `lbf-prelude` schema package and its associated `lbr-prelude` runtime package.

LambdaBuffers Prelude package provides:

1. Eq and Json type class and web-compatible Opaque types,
2. Eq implementation derivation (provided by `lbg-haskell`),
3. Json implementation derivation (provided by `lbg-haskell`),
4. Eq instances for Opaques (provided by `base` and friends),
5. Json instances for Opaques (provided by `lbr-prelude`),

## ./api

Contains the schema for the golden files in `./golden`.

## ./golden

1. `.json` golden files
   - For each `opaque` type defined in `lbf-prelude` that has a `Json` class 'instance' declared for it (tests 5).
   - For each 'transparent' type defined in the `./api` that has a `Json` class 'derive' rule declared for it (tests 3).

To regenerate goldens remove all the files in this directory and run:

```shell
lbt-prelude-golden generate ./golden
```

## ./lbt-prelude-haskell

A Haskell testsuite that serves as a 'reference implementation' which means it implements the `lbt-prelude-golden` CLI.

Tests performed:

1. Golden tests
   - `forall (golden : '*.json'). (toJson . fromJson) golden == golden`
2. Property tests
   - `forall (x : Foo.*): x == x:`
