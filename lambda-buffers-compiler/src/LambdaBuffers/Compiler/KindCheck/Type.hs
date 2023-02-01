module LambdaBuffers.Compiler.KindCheck.Type (Type (Var, Abs, App)) where

import LambdaBuffers.Compiler.KindCheck.Variable (Var)
import Prettyprinter (Doc, Pretty (pretty), parens, (<+>))
import Test.QuickCheck (Arbitrary, arbitrary, oneof, sized)

data Type
  = Var Var
  | App Type Type
  | Abs Var Type
  deriving stock (Eq, Show)

instance Pretty Type where
  pretty = \case
    Var a -> pretty a
    App t1 t2 -> show' t1 <> " " <> show' t2
    Abs a t1 -> "λ" <> pretty a <> "." <> pretty t1
    where
      show' :: Type -> Doc ann
      show' = \case
        Var a -> pretty a
        App t1 t2 -> parens $ show' t1 <+> show' t2
        Abs a t1 -> parens $ "λ" <> pretty a <> "." <> show' t1

instance Arbitrary Type where
  arbitrary = sized f
    where
      f n
        | n <= 0 = Var <$> arbitrary
        | otherwise =
            oneof
              [ Var <$> arbitrary
              , App <$> f (n `div` 2) <*> f (n `div` 2)
              , Abs <$> arbitrary <*> f (n - 1)
              ]
