# lbt-plutus

Testsuite for `lbf-plutus` schema package and its associated `lbr-plutus` runtime package.

## ./api

Contains the schema for the files in `./golden`.

## ./golden

Contains golden `.json` files for each `opaque` type defined in `lbf-plutus` that has a `Plutus.Json` class 'instances' declared for it.

To regenerate goldens remove all the files in this directory and run:

```shell
lbt-plutus-golden generate ./golden
```

## ./haskell-golden

A Haskell testsuite that serves as a 'reference implementation' which means it implements the `lbt-plutus-golden` CLI.

It also performs tests to guarantee properties for `Plutus.Json` and `Plutus.Eq` class hold for both 'instance' and 'derive' rules (`./api`).
