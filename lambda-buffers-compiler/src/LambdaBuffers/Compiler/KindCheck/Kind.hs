module LambdaBuffers.Compiler.KindCheck.Kind where

import LambdaBuffers.Compiler.KindCheck.Atom
import Prettyprinter

infixr 8 :->:

data Kind
  = Type
  | Kind :->: Kind
  | KVar Atom
  deriving stock (Eq, Show)

instance Pretty Kind where
  pretty = \case
    Type -> "*"
    ((x :->: y) :->: z) -> parens (pretty $ x :->: y) <+> "→" <+> pretty z
    x :->: y -> pretty x <+> "→" <+> pretty y
    KVar a -> pretty a
