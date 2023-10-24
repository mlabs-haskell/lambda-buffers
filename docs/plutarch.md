# LambdaBuffers for Plutarch

[Plutarch](https://github.com/Plutonomicon/plutarch-plutus) is a typed eDSL in Haskell for writing efficient Plutus Core validators.

## Type definition mapping

Plutarch backend support all types from the LB Plutus module, as to enable full ffeatured Plutus script development. However, it also support some type from the LB Prelude module, namely `Integer`, `Maybe`, `Either` and `List`.

```lbf
module Foo

import Prelude
import Plutus

sum FooSum a b = Bar a (Maybe Address) | Baz b (Maybe AssetClass)
derive Eq (FooSum a b)
derive Json (FooSum a b)
derive PlutusData (FooSum a b)

prod FooProd a b = a (Maybe Address) b (Maybe AssetClass)
derive Eq (FooProd a b)
derive Json (FooProd a b)
derive PlutusData (FooProd a b)

prod FooRec a b = {
  bar : a (Maybe Address),
  baz: b (Maybe AssetClass)
  }
derive Eq (FooRec a b)
derive Json (FooRec a b)
derive PlutusData (FooRec a b)
```

```haskell
module LambdaBuffers.Plutarch.Foo where

import Plutarch

data FooSum (a :: PType) (b :: PType) (s :: S) = FooSum'Bar (Term s a) (Term s (PMaybe PAddress))
    | FooSum'Baz (Term s b) (Term s (PMaybe PAssetClass))

data FooProd (a :: PType) (b :: PType) (s :: S) = FooProd (Term s a) (Term s (PMaybe PAddress)) (Term s b) (Term s (PMaybe PAssetClass))

data FooRec (a :: PType) (b :: PType) (s :: S) = FooRec (Term s a) (Term s (PMaybe PAddress)) (Term s b) (Term s (PMaybe PAssetClass))
```

## Type class implementations

Plutarch has a couple of fundamental classes essential to its operations.
Namely, `PlutusType`, `PIsData`, `PTryFrom` and `PEq`.

### PlutusType - (de)constructing Plutarch terms

[PlutusType](https://github.com/Plutonomicon/plutarch-plutus/blob/c14ad83479706566fe22e7b7b50b696043326c8f/Plutarch/Internal/PlutusType.hs#L56) serves to construct Plutarch eDSL terms from Haskell 'native' terms.

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

However, in LambdaBuffers, both of these methods follow the exact same logic pattern, and they correspond and can be generated using the `from Plutus data` specification.

```haskell
data FooTrivial (s :: S) = FooTrivial

instance PlutusType FooTrivial where
  type PInner FooTrivial = PData
  pcon' FooTrivial = lvToPlutusData (lvIntE 0)
  pmatch' pd f = pcaseInt
            # (pAsInt pd)
            # (lvListE [lvTupleE 0 (f FooTrivial)])
            # (ptraceError "Got PlutusData Integer but invalid value")
```

Note that `pmatch'` doesn't really have to case match on PlutusData as `ptryFrom` has to, we can assume its the current representation.

### PTryFrom - parsing Data into Plutarch terms

[PTryFrom](https://github.com/Plutonomicon/plutarch-plutus/blob/c14ad83479706566fe22e7b7b50b696043326c8f/Plutarch/TryFrom.hs#L73) serves to convert between Plutarch types. Note that's a fairly general use case, and we generally use this class in a very narrow form to specify how `PData` is 'parsed' into a Plutarch type.

```haskell
class PSubtype a b => PTryFrom (a :: PType) (b :: PType) where
  type PTryFromExcess a b :: PType
  type PTryFromExcess a b = PTryFromExcess a (PInner b)
  ptryFrom' :: forall s r. Term s a -> ((Term s b, Reduce (PTryFromExcess a b s)) -> Term s r) -> Term s r
  default ptryFrom' :: forall s r. (PTryFrom a (PInner b), PTryFromExcess a b ~ PTryFromExcess a (PInner b)) => Term s a -> ((Term s b, Reduce (PTryFromExcess a b s)) -> Term s r) -> Term s r
  ptryFrom' opq f = ptryFrom @(PInner b) @a opq \(inn, exc) -> f (punsafeCoerce inn, exc)
```

There's some additionally features exhibited by this type class, most noteworthy is the `PTryFromExcess` type family that enables us specify the part of the structure that wasn't parsed and is left unexamined. It's a form of optimization that becomes very important if you have a very complex data type such as `ScriptContext` from the `plutus-ledger-api`.
Apparently, a good intuition pump for the this 'excess' business is that of a [zipper](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf). We focus on a certain part of a data structure, only ever providing links to other parts that are left un-examined.

LambdaBuffers doesn't use this feature and sets the `PTryFromExcess` to a unit type, signaling that nothing is left unexamined.

```haskell
instance PTryFrom PData FooTrivial where
  type PTryFromExcess PData FooTrivial = Const ()
  ptryFrom' pd f =
    pcasePlutusData
      (plam $ \_pdCons -> ptraceError "Got PlutusData Constr")
      (plam $ \_pdList -> ptraceError "Got PlutusData List")
      ( plam $ \pdInt ->
          pcaseInt
            # pdInt
            # (lvListE [lvTupleE 0 (f (pcon FooTrivial, ()))])
            # (ptraceError "Got PlutusData Integer but invalid value")
      )
      (plam $ \_ -> ptraceError "Got unexpected PlutusData value")
      pd
```

Notice the difference from `pmatch'` implementation. It case matches on the provided PlutusData value, as it must assume it can be anything and errors if it encounters something unexpected.

Additionally, the continuation function receives the `pcon'` constructed Plutarch value (`Term`), rather than the Haskell 'native' value.

### PIsData - tracking 'is it plutus data encoded?' with types

[PIsData](https://github.com/Plutonomicon/plutarch-plutus/blob/c14ad83479706566fe22e7b7b50b696043326c8f/Plutarch/Builtin.hs#L354) TODO.

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
