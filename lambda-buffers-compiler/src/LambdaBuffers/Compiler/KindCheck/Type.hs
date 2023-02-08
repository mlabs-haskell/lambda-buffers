{-# LANGUAGE PatternSynonyms #-}

module LambdaBuffers.Compiler.KindCheck.Type (
  Type (Var, Abs, App),
  pattern Σ,
  pattern Π,
) where

import LambdaBuffers.Compiler.KindCheck.Variable (Variable (LocalRef))
import Prettyprinter (Doc, Pretty (pretty), parens, (<+>))
import Test.QuickCheck (Arbitrary, Gen, arbitrary, oneof, sized)

data Type
  = Var Variable
  | App Type Type
  | Abs Variable Type
  deriving stock (Eq, Show)

pattern Σ :: Type -> Type -> Type
pattern Σ t1 t2 = App (App (Var (LocalRef "Σ")) t1) t2

pattern Π :: Type -> Type -> Type
pattern Π t1 t2 = App (App (Var (LocalRef "Π")) t1) t2

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
      f :: Integral a => a -> Gen Type
      f n
        | n <= 0 = Var <$> arbitrary
        | otherwise =
            oneof
              [ Var <$> arbitrary
              , App <$> f (n `div` 2) <*> f (n `div` 2)
              , Abs <$> arbitrary <*> f (n - 1)
              ]
