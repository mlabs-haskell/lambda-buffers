module LambdaBuffers.Compiler.KindCheck.Type (
  Type (Abs, VoidT, Product, Sum, Constructor, Opaque, Var, UnitT, App),
  tyProd,
  tyUnit,
  tySum,
  tyVoid,
) where

import LambdaBuffers.Compiler.KindCheck.Variable (Variable)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC (
  Constructor,
  SourceInfo,
  TyAbs,
 )
import Prettyprinter (Pretty (pretty), viaShow)

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

-- \case
-- Var a -> pretty a
-- App t1 t2 -> show' t1 <> " " <> show' t2
-- Abs a t1 -> "λ" <> pretty a <> "." <> pretty t1
-- where
--   show' :: Type -> Doc ann
--   show' = \case
--     Var a -> pretty a
--     App t1 t2 -> parens $ show' t1 <+> show' t2
--     Abs a t1 -> parens $ "λ" <> pretty a <> "." <> show' t1

-- instance Arbitrary Type where
--   arbitrary = sized f
--     where
--       f :: Integral a => a -> Gen Type
--       f n
--         | n <= 0 = Var <$> arbitrary
--         | otherwise =
--             oneof
--               [ Var <$> arbitrary
--               , App <$> f (n `div` 2) <*> f (n `div` 2)
--               , Abs <$> sized (n-1) arbitrary <*> f (n - 1)
--               , VoidT
--               , Product
--               , App
--               , Sum
--               , Constructor
--               , Opaque
--               , Var
--               , UnitT
--               ]
