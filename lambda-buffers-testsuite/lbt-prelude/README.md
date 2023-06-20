# lbt-prelude

Testsuite for `lbf-prelude` schema package and its associated `lbr-prelude` runtime package.

## ./api

Contains the schema for the files in `./golden`.

## ./golden

Contains golden `.json` files for each `opaque` type defined in `lbf-prelude` that has a `Prelude.Json` class 'instances' declared for it.

To regenerate goldens remove all the files in this directory and run:

```shell
lbt-prelude-golden generate ./golden
```

## ./haskell-golden

A Haskell testsuite that serves as a 'reference implementation' which means it implements the `lbt-prelude-golden` CLI.

It also performs tests to guarantee properties for `Prelude.Json` and `Prelude.Eq` class hold for both 'instance' and 'derive' rules (`./api`).
