module LambdaBuffers.Codegen.LamVal (
  ValueE (..),
  ValueName,
  Ref,
  (@),
  SumImpl,
  ProductImpl,
  RecordImpl,
  Record,
  Product,
  Sum,
  Field,
  Ctor,
  QField,
  QRecord,
  QProduct,
  QSum,
  QCtor,
) where

import Data.Map.Ordered (OMap)
import Data.Text (Text)
import LambdaBuffers.Compiler.LamTy qualified as LT
import LambdaBuffers.ProtoCompat qualified as PC

-- | Simple HOAS encoding of a little lambda calculus language.
data ValueE where
  LamE :: (ValueE -> ValueE) -> ValueE
  AppE :: ValueE -> ValueE -> ValueE
  RefE :: Ref -> ValueE
  VarE :: String -> ValueE
  -- | Sum expressions
  CaseE :: QSum -> ValueE -> ((Ctor, [ValueE]) -> ValueE) -> ValueE
  CtorE :: QCtor -> [ValueE] -> ValueE
  -- | Record expressions
  RecordE :: QRecord -> [(Field, ValueE)] -> ValueE
  FieldE :: QField -> ValueE -> ValueE
  -- | Product expressions
  ProductE :: QProduct -> [ValueE] -> ValueE
  LetE :: QProduct -> ValueE -> ([ValueE] -> ValueE) -> ValueE
  -- | Int expressions
  IntE :: Int -> ValueE
  CaseIntE :: ValueE -> [(ValueE, ValueE)] -> (ValueE -> ValueE) -> ValueE
  -- | Text expressions
  TextE :: Text -> ValueE
  CaseTextE :: ValueE -> [(ValueE, ValueE)] -> (ValueE -> ValueE) -> ValueE
  -- | List expressions
  ListE :: [ValueE] -> ValueE
  CaseListE :: ValueE -> [(Int, [ValueE] -> ValueE)] -> (ValueE -> ValueE) -> ValueE
  -- | Tuple expressions
  TupleE :: ValueE -> ValueE -> ValueE
  -- | Error
  ErrorE :: String -> ValueE

(@) :: ValueE -> ValueE -> ValueE
(@) = AppE

-- | A name for a value.
type ValueName = String

{- | A value reference that may be parametrized on `LT.Ty`s.

 We use this during code generation when 'finding' concrete values, especially for polymorphic functions.
 For example

 ```haskell
 id :: a -> a
 id x = x

 firstUseSite :: Int -> Int
 firstUseSite = id

 secondUseSite :: String -> String
 secondUseSite = id

 thirdUseSite :: a -> a
 thirdUseSite = id
 ```

 In the `firstUseSite` the `id` is instantiated to 'is `(["Int"], "id")`.
 In the `secondUseSite` the `id` is instantiated to 'is `(["String"], "id")`.
 In the `thirdUseSite` the `id` is instantiated to 'is `(["a"], "id")`.
-}
type Ref = ([LT.Ty], ValueName)

{- | Wrapper types.
 TODO(bladyjoker): Refactor `ProtoCompat.Eval` to have these types explicitly named.
-}
type Sum = OMap (PC.InfoLess PC.ConstrName) LT.Ty

type Product = [LT.Ty]
type Record = OMap (PC.InfoLess PC.FieldName) LT.Ty

type Ctor = (PC.InfoLess PC.ConstrName, Product)
type Field = (PC.InfoLess PC.FieldName, LT.Ty)

type SumImpl = QSum -> ValueE
type ProductImpl = QProduct -> ValueE
type RecordImpl = QRecord -> ValueE

type QCtor = (PC.QTyName, Ctor)
type QSum = (PC.QTyName, Sum)

type QField = (PC.QTyName, PC.InfoLess PC.FieldName)
type QRecord = (PC.QTyName, Record)

type QProduct = (PC.QTyName, Product)
