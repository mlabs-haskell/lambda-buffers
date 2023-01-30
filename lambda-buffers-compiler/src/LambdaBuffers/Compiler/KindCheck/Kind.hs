module LambdaBuffers.Compiler.KindCheck.Kind (Kind (KindP, (:->:), KVar), KindPrimitive (Type, Constraint)) where

import LambdaBuffers.Compiler.KindCheck.Variable (Atom)
import Prettyprinter (Pretty (pretty), parens, (<+>))

infixr 8 :->:

data KindPrimitive = Type | Constraint
  deriving stock (Eq, Show)

data Kind
  = KindP KindPrimitive
  | Kind :->: Kind
  | KVar Atom
  deriving stock (Eq, Show)

instance Pretty Kind where
  pretty = \case
    KindP k -> pretty k
    ((x :->: y) :->: z) -> parens (pretty $ x :->: y) <+> "→" <+> pretty z
    x :->: y -> pretty x <+> "→" <+> pretty y
    KVar a -> pretty a

instance Pretty KindPrimitive where
  pretty = \case
    Type -> "Type"
    Constraint -> "Constraint"
