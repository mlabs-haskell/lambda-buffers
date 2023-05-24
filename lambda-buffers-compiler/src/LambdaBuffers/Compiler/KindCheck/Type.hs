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
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Pretty (pretty), viaShow)

-- NOTE(cstml): Let's remove the Arbitrary instances and replaces them with
-- Gens.

data Variable
  = -- | All TyRefs are fully qualified. The context determines if they're local
    -- or not.
    QualifiedTyRef PC.ForeignRef
  | TyVar PC.VarName
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (SOP.Generic)

instance Pretty Variable where
  pretty = viaShow

instance PC.InfoLessC Variable

instance Pretty (PC.InfoLess Variable) where
  pretty x = PC.withInfoLess x pretty

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
