module LambdaBuffers.Compiler.KindCheck.Type (
  Type (Abs, VoidT, Product, Sum, Constructor, Opaque, Var, UnitT, App),
  tyProd,
  tyUnit,
  tySum,
  tyVoid,
  Variable (TyVar, QualifiedTyRef),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import LambdaBuffers.Compiler.ProtoCompat.InfoLess (InfoLess, InfoLessC, withInfoLess)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Pretty (pretty), viaShow)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary (GenericArbitrary))

data Variable
  = -- | All TyRefs are fully qualified. The context determines if they're local
    -- or not.
    QualifiedTyRef PC.ForeignRef
  | TyVar PC.VarName
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericArbitrary Variable
  deriving anyclass (SOP.Generic)

instance Pretty Variable where
  pretty = viaShow

instance InfoLessC Variable

instance Pretty (InfoLess Variable) where
  pretty x = withInfoLess x pretty

data Type
  = Abs PC.TyAbs
  | App Type Type
  | Var Variable
  | Product Type Type
  | Sum Type Type
  | Constructor PC.Constructor
  | Opaque PC.SourceInfo
  | UnitT
  | VoidT
  deriving stock (Eq, Show)

tyProd :: Type -> Type -> Type
tyProd = Product

tySum :: Type -> Type -> Type
tySum = Sum

tyUnit :: Type
tyUnit = UnitT

tyVoid :: Type
tyVoid = VoidT

instance Pretty Type where
  pretty = viaShow
