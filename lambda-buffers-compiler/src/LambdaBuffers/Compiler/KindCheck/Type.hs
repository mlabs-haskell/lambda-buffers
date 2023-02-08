{-# LANGUAGE PatternSynonyms #-}

module LambdaBuffers.Compiler.KindCheck.Type (
  Type (Var, Abs, App),
  pattern Σ,
  pattern Π,
) where

import LambdaBuffers.Compiler.KindCheck.Variable (Variable (LocalRef))
import Prettyprinter (Doc, Pretty (pretty), parens, (<+>))

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
