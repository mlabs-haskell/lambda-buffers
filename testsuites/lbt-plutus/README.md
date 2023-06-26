# lbt-plutus

Testsuite for `lbf-plutus` schema package and its associated `lbr-plutus` runtime package.

LambdaBuffers Plutus package provides:

1. PlutusData encoding type class and `plutus-ledger-api` Opaque types,
2. PlutusData implementation derivation (provided by `lbg-haskell`),
3. PlutusData instances for Opaques (provided by `plutus-ledger-api` and `plutus-tx`),
4. Json instances for Opaques (provided by `lbr-plutus`),
5. Eq instances for Opaques (provided by `plutus-ledger-api` and `plutus-tx`).

## ./api

Contains the schema for the golden files in `./golden`.

## ./golden

1. `.json` golden files
   - For each `opaque` type defined in `lbf-plutus` that has a `Json` class 'instance' declared for it (tests 4).
2. `.pd.json` golden files
   - For each `opaque` type defined in `lbf-plutus` that has a `PlutusData` class 'instance' rule declared for it (tests 3),
   - For each 'transparent' type defined in the `./api` that has a `PlutsData` class 'derive' rule declared for it (tests 2).

To regenerate goldens remove all the files in this directory and run:

```shell
lbt-plutus-golden generate-json ./golden
lbt-plutus-golden generate-plutusdata ./golden
```

## ./lbt-plutus-haskell

A Haskell testsuite that serves as a 'reference implementation' which means it implements the `lbt-plutus-golden` CLI.

Tests performed:

1. Golden tests
   - `forall (golden : '*.json'). (toJson . fromJson) golden == golden`
   - `forall (golden : '*.pd.json'). (toJson . toPlutusData . fromPlutusData . fromJson) golden == golden`
2. Property tests
   - `forall (x : Foo.*): (fromPlutusData . toPlutusData) x == x:`
