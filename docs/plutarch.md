# LambdaBuffers for Plutarch

[Plutarch](https://github.com/Plutonomicon/plutarch-plutus) is a typed eDSL in Haskell for writing efficient Plutus Core validators.

LambdaBuffers creates Plutarch type definitions and associated Plutarch type class implementations for [PlutusType](#plutustype), [PIsData](#pisdata) and [PShow](#pshow) classes.

Additionally, when instructed by a LambdaBuffers `derive` statement type class implementations for [PEq](#peq) and [PTryFrom](#ptryfrom) are also printed.

A small example:

```shell
❯ nix develop github:mlabs-haskell/lambda-buffers#dev-plutarch
❯ cat > Example.lbf
module Example

import Prelude
import Plutus.V1 (PlutusData, AssetClass)

record Example a = {
  foo : AssetClass,
  bar : a
  }

derive Eq (Example a)
derive Json (Example a)
derive PlutusData (Example a)

❯ lbf-plutus-to-plutarch Example.lbf
[lbf][INFO] Compilation OK
[lbf][INFO] Codegen OK

❯ find autogen/
autogen/
autogen/build.json
autogen/LambdaBuffers
autogen/LambdaBuffers/Example
autogen/LambdaBuffers/Example/Plutarch.hs
```

For a full example see [Example](#example).

## LambdaBuffers modules

Writing .lbf schemas with API types intended for Plutarch backend will typically use the following LambdaBuffers schema modules:

1. [Prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-prelude/Prelude.lbf),
2. [Plutus.V1](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-plutus/Plutus/V1.lbf),
3. [Plutus.V2](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-plutus/Plutus/V2.lbf).
4. [Plutus.V3](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-plutus/Plutus/V3.lbf).

Take a look at [Example.lbf](https://github.com/mlabs-haskell/lambda-buffers/tree/main/docs/plutarch/api/Example.lbf) schema as an example.

## Haskell libraries

The necessary LambdaBuffers runtime libraries a typical Plutarch project needs when working with LambdaBuffers:

1. [lbr-plutarch](https://github.com/mlabs-haskell/lambda-buffers/tree/main/runtimes/haskell/lbr-plutarch) a Haskell runtime library necessary for working with `lbf-xyz` libraries.
2. [lbf-prelude-plutarch](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-prelude) that contains the [LambdaBuffers Prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-prelude) schema library generated by LambdaBuffers.
3. [lbf-plutus-plutarch](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-plutus) that contains the [LambdaBuffers Plutus](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-plutus) schema library generated by LambdaBuffers.

Of course, additional imports for Plutarch libraries are also necessary [plutarch](https://github.com/Plutonomicon/plutarch-plutus) and optionally [plutarch-extra](https://github.com/Plutonomicon/plutarch-plutus/tree/master/plutarch-extra).

For a full example see [Example](#example).

### Inspecting the generated output

You can inspect the generated libraries using Nix:

```shell
❯ nix build .#lbf-prelude-plutarch
❯ find result/autogen/
result/autogen/
result/autogen/LambdaBuffers
result/autogen/LambdaBuffers/Prelude
result/autogen/LambdaBuffers/Prelude/Plutarch.hs

❯ nix build .#lbf-plutus-plutarch
❯ find result/autogen/
result/autogen/
result/autogen/LambdaBuffers
result/autogen/LambdaBuffers/Plutus
result/autogen/LambdaBuffers/Plutus/V3
result/autogen/LambdaBuffers/Plutus/V3/Plutarch.hs
result/autogen/LambdaBuffers/Plutus/V2
result/autogen/LambdaBuffers/Plutus/V2/Plutarch.hs
result/autogen/LambdaBuffers/Plutus/V1
result/autogen/LambdaBuffers/Plutus/V1/Plutarch.hs
```

## Haskell modules

The set of imports a Plutarch program using LambdaBuffers would typically need is the following:

```haskell
import LambdaBuffers.Plutus.V1.Plutarch ()
import LambdaBuffers.Plutus.V2.Plutarch ()
import LambdaBuffers.Plutus.V3.Plutarch ()
import LambdaBuffers.Prelude.Plutarch ()
import LambdaBuffers.Runtime.Plutarch ()
import Plutarch.Prelude ()
import Plutarch.LedgerApi.V1 ()
import Plutarch.LedgerApi.V2 ()
import Plutarch.LedgerApi.V3 ()
```

1. LambdaBuffers.Plutus.V1.Plutarch is a module generated from [Plutus.V1](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-plutus/Plutus/V1.lbf) LambdaBuffers schema and provided by the [lbf-plutus-plutarch](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-plutus) runtime library.
2. LambdaBuffers.Plutus.V2.Plutarch is a module generated from [Plutus.V2](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-plutus/Plutus/V2.lbf) LambdaBuffers schema and provided by the [lbf-plutus-plutarch](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-plutus) runtime library.
3. LambdaBuffers.Plutus.V3.Plutarch is a module generated from [Plutus.V3](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-plutus/Plutus/V2.lbf) LambdaBuffers schema and provided by the [lbf-plutus-plutarch](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-plutus) runtime library.
4. LambdaBuffers.Prelude.Plutarch is a module generated from [Prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-prelude/Prelude.lbf) LambdaBuffers schema and provided by the [lbf-prelude-plutarch](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-prelude) runtime library.
5. LambdaBuffers.Runtime.Plutarch is a module provided by the [lbr-plutarch](https://github.com/mlabs-haskell/lambda-buffers/tree/main/runtimes/haskell/lbr-plutarch) runtime library.

> Generated Plutarch module for a LambdaBuffers schema `Foo/Bar.lbf` (ie. `Foo.Bar`) is stored at `Foo/Bar/Plutarch.hs`

## Restrictions

Plutarch backend doesn't support recursive type definitions unfortunately (see #131).

The following will not work:

```lbf
module ModuleWithRecursiveType

import Prelude (Eq)
import Plutus.V1 (PlutusData)

sum List a = Cons a (List a) | Nil
derive Eq (List a)
derive PlutusData (List a)
```

Additionally, LambdaBuffers record types are mapped to Plutarch product types:

```lbf
module ModuleWithARecordType

import Prelude (Eq, Integer, Bool)
import Plutus.V1 (PlutusData)

record Foo = {
  bar: Integer,
  baz: Bool
  }
derive Eq Foo
derive PlutusData Foo
```

Essentially, the record definitions are 'degraded' into product types such that the order of product fields is the order of record fields as they are defined at source.

For example the `Foo` record defined above would have no difference in Plutarch if it was defined as product `Foo` below:

```lbf
prod Foo = Integer Bool
```

The Plutarch backend doesn't support the use of `Char`, `Text`, `Bytes` (there's a Plutus.V1.Bytes), `Set` and `Map` (there's a Plutus.V1.Map) from [LambdaBuffers Prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-prelude/Prelude.lbf) module.

## Plutarch

### Type definition mapping

Plutarch backend supports all types from the [LambdaBuffers Plutus](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-plutus) schema library, as to enable full featured Plutus script development.

Additionally, it also supports some types from the [LambdaBuffers Prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/main/libs/lbf-prelude) schema library, namely `Bool`, `Integer`, `Maybe`, `Either` and `List`.

```lbf
module Foo

sum Sum = Some a | Nothing

record Record a = {
  foo : Bytes,
  bar: a
}

prod Product a = Bytes a
```

translates into Plutarch equivalent:

```haskell
module LambdaBuffers.Foo.Plutarch (Sum(..), Record(..), Product(..)) where

import qualified LambdaBuffers.Plutus.V1.Plutarch
import qualified LambdaBuffers.Prelude.Plutarch
import qualified LambdaBuffers.Runtime.Plutarch
import qualified Plutarch.Prelude
import qualified Plutarch.Internal.PlutusType
import qualified Plutarch.Unsafe

data Sum (a :: PType) (s :: Plutarch.Prelude.S) = Sum'Some (Plutarch.Prelude.Term s (Plutarch.Prelude.PAsData LambdaBuffers.Plutus.V1.Plutarch.Bytes)) (Plutarch.Term s (Plutarch.Prelude.PAsData PAsData))
                                  | Sum'Nothing
  deriving stock GHC.Generics.Generic
  deriving anyclass Plutarch.Prelude.PShow

data Record (a :: PType) (s :: Plutarch.Prelude.S) = Record (Plutarch.Prelude.Term s (Plutarch.Prelude.PAsData LambdaBuffers.Plutus.V1.Plutarch.Bytes)) (Plutarch.Term s (Plutarch.Prelude.PAsData PAsData))
  deriving stock GHC.Generics.Generic
  deriving anyclass Plutarch.Prelude.PShow

data Product (a :: PType) (s :: Plutarch.Prelude.S) = Product (Plutarch.Prelude.Term s (Plutarch.Prelude.PAsData LambdaBuffers.Plutus.V1.Plutarch.Bytes)) (Plutarch.Term s (Plutarch.Prelude.PAsData PAsData))
  deriving stock GHC.Generics.Generic
  deriving anyclass Plutarch.Prelude.PShow
```

### Type class implementations

Plutarch has a couple of fundamental type classes essential to its operations namely, `PlutusType`, `PIsData`, `PTryFrom` and `PEq`.

#### PlutusType

Printing an implementation for this class for a particular type is governed by `derive Plutus.V1.PlutusData <type>` statements in .lbf schemas.

[PlutusType](https://github.com/Plutonomicon/plutarch-plutus/blob/c14ad83479706566fe22e7b7b50b696043326c8f/Plutarch/Internal/PlutusType.hs#L56) serves to (de)construct Plutarch eDSL terms from Haskell 'native' terms.

```haskell
class PlutusType (a :: PType) where
  type PInner a :: PType
  pcon' :: forall s. a s -> Term s (PInner a)
  pmatch' :: forall s b. Term s (PInner a) -> (a s -> Term s b) -> Term s b
```

Additionally, Plutarch enables specifying terms to have different 'value' representation, like Scott encoded terms or PlutusData encoded terms.
This is what the `PInner` type family is used to specify.
LambdaBuffers only cares about `PlutusData` encoded terms since we're using it to specify Plutus datum structures.

The task is to generate a `pcon'` implementation such that we can construct Plutarch `Term`s that have some `PInner` representation of type `PData`, from Haskell 'native' values.
The `pcon'` implementation must match the LB Plutus PlutusData encoding class standard, and so we'll use the same 'to Plutus data' specification to generate `pcon'` implementations.

Constructing is always only one part of the story, there's also deconstruction that is captured by the `pmatch'` method.
This method serves to 'pattern match' on a value that was already constructed using `pcon'` and dispatch said value to a provided continuation function.
It's important to note that there's a subtle but important distinction to be made between the `ptryFrom` and `pmatch'` methods.
`pmatch'` assumes that the value it receives is indeed correct, as it was constructed using the `pcon'` method.
This means that `pmatch'` should never error, and if it does that means the implementation is wrong.
`ptryFrom` is different, as it takes some `PData` and tries to parse it into a `PType`, but can fail.

However, in LambdaBuffers, both of these methods follow the exact same logical pattern, and they correspond and can be generated using the `from Plutus data` specification.

#### PTryFrom

Printing an implementation for this class for a particular type is governed by `derive Plutus.V1.PlutusData <type>` statements in .lbf schemas.

[PTryFrom](https://github.com/Plutonomicon/plutarch-plutus/blob/c14ad83479706566fe22e7b7b50b696043326c8f/Plutarch/TryFrom.hs#L73) serves specify how `PData` is 'parsed' into a Plutarch type.
N
It's generally used to convert between Plutarch types, but that's a fairly general use case, and we generally use this class in a very narrow form to specify how `PData` is 'parsed' into a Plutarch type.

```haskell
class PSubtype a b => PTryFrom (a :: PType) (b :: PType) where
  type PTryFromExcess a b :: PType
  type PTryFromExcess a b = PTryFromExcess a (PInner b)
  ptryFrom' :: forall s r. Term s a -> ((Term s b, Reduce (PTryFromExcess a b s)) -> Term s r) -> Term s r
  default ptryFrom' :: forall s r. (PTryFrom a (PInner b), PTryFromExcess a b ~ PTryFromExcess a (PInner b)) => Term s a -> ((Term s b, Reduce (PTryFromExcess a b s)) -> Term s r) -> Term s r
  ptryFrom' opq f = ptryFrom @(PInner b) @a opq \(inn, exc) -> f (punsafeCoerce inn, exc)
```

There's some additionally features exhibited by this type class, most noteworthy is the `PTryFromExcess` type family that enables us specify the part of the structure that wasn't parsed and is left unexamined.
It's a form of optimization that becomes very important if you have a very complex data type such as `ScriptContext` from the `plutus-ledger-api`.

Apparently, a good intuition pump for this 'excess' business is that of a [zipper](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf).
We focus on a certain part of a data structure, only ever providing links to other parts that are left un-examined.

LambdaBuffers doesn't use this feature and sets the `PTryFromExcess` to a unit type, signaling that nothing is left unexamined.

#### PIsData

Printing an implementation for this class for a particular type is governed by `derive Plutus.V1.PlutusData <type>` statements in .lbf schemas.

[PIsData](https://github.com/Plutonomicon/plutarch-plutus/blob/c14ad83479706566fe22e7b7b50b696043326c8f/Plutarch/Builtin.hs#L354) serves to track 'is it Plutus data encoded?' with types.

```haskell
newtype PAsData (a :: PType) (s :: S) = PAsData (Term s a)

class PIsData a where
  pfromDataImpl :: Term s (PAsData a) -> Term s a
  default pfromDataImpl :: PIsData (PInner a) => Term s (PAsData a) -> Term s a
  pfromDataImpl x = punsafeDowncast $ pfromDataImpl (punsafeCoerce x :: Term _ (PAsData (PInner a)))

  pdataImpl :: Term s a -> Term s PData
  default pdataImpl :: PIsData (PInner a) => Term s a -> Term s PData
  pdataImpl x = pdataImpl $ pto x
```

```haskell
instance PIsData FooTrivial where
  pdataImpl = punsafeCoerce
  pfromDataImpl = punsafeCoerce

instance PEq FooTrivial where
  (#==) = \l r -> pdata l #== pdata r
```

> Due to generated types having a `PAsData` attached to them, be ready to use `pdata` and `pfromData` to switch between forms.

#### PEq

Printing an implementation for this class for a particular type is governed by `derive Prelude.Eq <type>` statements in .lbf schemas.

[PEq](https://github.com/Plutonomicon/plutarch-plutus/blob/c14ad83479706566fe22e7b7b50b696043326c8f/Plutarch/Bool.hs#L74) serves to track provide equality checks to Plutarch types.

```haskell
class PEq t where
  (#==) :: Term s t -> Term s t -> Term s PBool
  default (#==) ::
    (PGeneric t, PlutusType t, All2 PEq (PCode t)) =>
    Term s t ->
    Term s t ->
    Term s PBool
  a #== b = gpeq # a # b

infix 4 #==
```

> We don't generate an implementation from the LambdaBuffers 'equality spec', rather we delegate the equality check to the underlying 'PData' representations that all generated types have for performance.

#### PShow

All generated types have a PShow instance derived using the internal Plutarch deriving mechanism.

[PShow](https://github.com/Plutonomicon/plutarch-plutus/blob/c14ad83479706566fe22e7b7b50b696043326c8f/Plutarch/Show.hs#L52) serves to stringify Plutarch types which is very useful during debugging.

## Example

Let work through the [Plutarch example](https://github.com/mlabs-haskell/lambda-buffers/tree/main/docs/plutarch) available in the repo.

First, please check the [Getting started](getting-started.md) guide on how to prepare to work with the repo and setup Nix.

Let's see what we have here:

```shell
lambda-buffers/docs/plutarch ❯ find
.
./build.nix
./cabal.project
./hie.yaml
./plutarch-example.cabal
./app
./app/Example.hs
./api
./api/Example.lbf
./.envrc
```

The salient bits we should focus on are:

1. The LambdaBuffers .lbf schema in [./api/Example.lbf](https://github.com/mlabs-haskell/lambda-buffers/tree/main/docs/plutarch/api/Example.lbf) that describes the API types used by our little program,
2. The Haskell Plutarch program in [./app/Example.hs](https://github.com/mlabs-haskell/lambda-buffers/tree/main/docs/plutarch/app/Example.hs) that works with the API types.

To inspect the generated library:

```shell
lambda-buffers/docs/plutarch ❯ nix build .#lbf-plutarch-example-api
lambda-buffers/docs/plutarch ❯ find autogen/
autogen/
autogen/build.json
autogen/LambdaBuffers
autogen/LambdaBuffers/Example
autogen/LambdaBuffers/Example/Plutarch.hs
```

> The name of the generated library `lbf-plutarch-example-api` is set in the ./plutarch/build.nix Nix build file.

However, it's not expected for users to need to do this. If you have any issue please reach out.

Inspecting the [Cabal file](https://github.com/mlabs-haskell/lambda-buffers/tree/main/docs/plutarch/plutarch-example.cabal) shows the standard runtime libraries we need:

```shell
lambda-buffers/docs/plutarch ❯ cabal info .
* plutarch-example-0.1.0.0 (program)
    Synopsis:      LambdaBuffers Plutarch example
    Versions available: [ Not available from server ]
    Versions installed: [ Unknown ]
    Homepage:      [ Not specified ]
    Bug reports:   [ Not specified ]
    License:       NONE
    Author:        Drazen Popovic
    Maintainer:    bladyjoker@gmail.com
    Source repo:   [ Not specified ]
    Executables:   plutarch-example
    Flags:         dev
    Dependencies:  base >=4.16, lbf-plutarch-example-api, lbf-plutus-plutarch,
                   lbf-prelude-plutarch, lbr-plutarch, plutarch, plutarch-ledger-api,
                   text >=1.2
    Cached:        Yes
```

Run the program:

```shell
lambda-buffers/docs/plutarch ❯ cabal run
"Friends, peace and love!!!"
```

Take a look at the [Example.hs](https://github.com/mlabs-haskell/lambda-buffers/tree/main/docs/plutarch/app/Example.hs) to see how generated types are used, namely how they are constructed with `pcon` and deconstructed with `pmatch` (or `pmatchC`).
