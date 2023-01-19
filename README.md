# Lambda Buffers

## Introduction

_LambdaBuffers_ is a schema language (similar to ProtoBuffers, ADL, ASN.1, JSON
Schema, etc.) and associated code generation toolkit. The goal of this project
is to provide developers tools to define algebraic data types in a
language-agnostic format such that shared data types can be declared in one
place while maintaining compatibility across a plethora of supported languages.

Users may refer to the [comparison matrix](./docs/comparison-matrix.md) for an
in-depth comparison of LambdaBuffers' features against the feature-set of other
popular schema-languages.

At a glance, you may wish to choose LambdaBuffers instead of one of its
competitors if your project requires:

 1. _Parameterized Data Types_: Unlike ProtoBuffers or JSON Schema,
    LambdaBuffers allows users to define algebraic data types which take type
    variable arguments. If your project's domain is most accurately represented
    by parameterized data types. LamdaBuffers may be a good choice for your
    needs.

 2. _Opaque Types_: Almost every competing schema language provides users a
    fixed set of builtin or primitive types, which are handled in a special
    manner by the code generation and cannot be extended. LambdaBuffers, by
    contrast, allows users to add their own builtin types and extend the
    existing code generation framework to handle those builtins in a manner
    intended by the users. There are no _special_ primitive types in
    LambdaBuffers; a user-defined primitive type is defined in exactly the same
    way (i.e. as an `opaque` type) as a LambdaBuffers "builtin".

 3. _Typeclass Support_: While nearly every schema language supports generating
    type definitions in supported target languages, to our knowledge no schema
    language supports generating commonly used functions that operate on those
    types. Unlike other schema languages, LambdaBuffers supports code generation
    for _typeclass instances_ (or the equivalent in languages that lack support
    for typeclasses) to reduce the amount of boilerplate required to
    productively make use of the generated types. While LambdaBuffers is still a
    work-in-progress, we expect that, upon completion, an extensive test suite
    will provide a high degree of assurance that the instances/methods generated
    by the LamdaBuffers code generator behave identically.

## Documentation

- [Compiler](./docs/compiler.md)

## Getting started

### Installing Nix

This repository relies heavily on the [Nix Package
Manager](https://nixos.org/download.html) for both development and package
distribution.

To install run the following command:

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

and follow the instructions.

```sh
$ nix --version
nix (Nix) 2.8.0
```

> NOTE: The repository should work with Nix version greater or equal to 2.8.0.

Make sure to enable [Nix Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes)
and IFD by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` on
your machine and add the following configuration entries:

```yaml
experimental-features = nix-command flakes
allow-import-from-derivation = true
```

Optionally, to improve build speed, it is possible to set up a binary caches
maintained by IOHK and Plutonomicon by setting additional configuration entries:

```yaml
substituters = https://cache.nixos.org https://iohk.cachix.org https://cache.iog.io https://public-plutonomicon.cachix.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=
```

### Building and development

To facilitate seamlessly moving between directories and associated Nix
development shells we use [direnv](https://direnv.net) and
[nix-direnv](https://github.com/nix-community/nix-direnv):

To install both using `nixpkgs`:

```sh
nix profile install nixpkgs#direnv
nix profile install nixpkgs#nix-direnv
```

Your shell and editors should pick up on the `.envrc` files in different directories and prepare the environment accordingly.
Use `direnv allow` to enable the direnv environment and `direnv reload` to reload it when necessary.

Additionally, throughout the repository one can use:

```sh
$ pre-commit run --all
cabal-fmt............................................(no files to check)Skipped
fourmolu.................................................................Passed
hlint....................................................................Passed
markdownlint.............................................................Passed
nix-linter...............................................................Passed
nixpkgs-fmt..............................................................Passed
shellcheck...........................................(no files to check)Skipped
typos....................................................................Passed
```

To run all the code quality tooling specified in the [pre-commit-check config file](./pre-commit-check.nix)
