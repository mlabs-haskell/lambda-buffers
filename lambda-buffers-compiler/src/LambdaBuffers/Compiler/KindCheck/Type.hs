module LambdaBuffers.Compiler.KindCheck.Type (Type (Var, Abs, App)) where

import LambdaBuffers.Compiler.KindCheck.Variable (Atom, Var)
import Prettyprinter (Doc, Pretty (pretty), parens, (<+>))

data Type
  = Var Var
  | App Type Type
  | Abs Atom Type
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
