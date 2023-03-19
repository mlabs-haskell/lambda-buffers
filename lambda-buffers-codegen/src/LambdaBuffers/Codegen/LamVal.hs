module LambdaBuffers.Codegen.LamVal (
  ValueE (..),
  ValueName,
  Ref,
  letE,
  fieldE,
  lamE,
  (@),
  caseE,
  refE,
  SumImpl,
  ProductImpl,
  RecordImpl,
  Record,
  Product,
  Sum,
  Field,
  Ctor,
) where

import Data.Map.Ordered (OMap)
import LambdaBuffers.Compiler.ProtoCompat.Eval qualified as E
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

-- | A name for a value.
type ValueName = String

-- | A value reference that may be parametrized on a `E.Ty`.
type Ref = (Maybe E.Ty, ValueName)

-- | Simple HOAS encoding of a little lambda calculus language.
data ValueE where
  LamE :: (ValueE -> ValueE) -> ValueE
  AppE :: ValueE -> ValueE -> ValueE
  RefE :: Ref -> ValueE
  VarE :: String -> ValueE
  CaseE :: (PC.QTyName, Sum) -> ValueE -> ((Ctor, [ValueE]) -> ValueE) -> ValueE
  FieldE :: (PC.QTyName, PC.InfoLess PC.FieldName) -> ValueE -> ValueE
  LetE :: (PC.QTyName, Product, ValueE) -> ([ValueE] -> ValueE) -> ValueE

letE :: (PC.QTyName, Product, ValueE) -> ([ValueE] -> ValueE) -> ValueE
letE = LetE

fieldE :: (PC.QTyName, PC.InfoLess PC.FieldName) -> ValueE -> ValueE
fieldE = FieldE

lamE :: (ValueE -> ValueE) -> ValueE
lamE = LamE

(@) :: ValueE -> ValueE -> ValueE
(@) = AppE

caseE :: (PC.QTyName, Sum) -> ValueE -> ((Ctor, [ValueE]) -> ValueE) -> ValueE
caseE = CaseE

refE :: Ref -> ValueE
refE = RefE

{- | Wrapper types.
 TODO(bladyjoker): Refactor `ProtoCompat.Eval` to have these types explicitly named.
-}
type Sum = OMap (PC.InfoLess PC.ConstrName) E.Ty

type Product = [E.Ty]
type Record = OMap (PC.InfoLess PC.FieldName) E.Ty

type Ctor = (PC.InfoLess PC.ConstrName, Product)
type Field = (PC.InfoLess PC.FieldName, E.Ty)

type SumImpl = PC.QTyName -> Sum -> ValueE
type ProductImpl = PC.QTyName -> Product -> ValueE
type RecordImpl = PC.QTyName -> Record -> ValueE
