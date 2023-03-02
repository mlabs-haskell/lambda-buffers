module LambdaBuffers.Compiler.KindCheck.Kind (Kind (KType, (:->:), KVar, KConstraint), Atom) where

import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty), parens, (<+>))
import Test.QuickCheck.Arbitrary.Generic (Arbitrary, GenericArbitrary (GenericArbitrary))

infixr 8 :->:

type Atom = Integer

data Kind
  = KType
  | Kind :->: Kind
  | KVar Atom
  | KConstraint
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericArbitrary Kind

instance Pretty Kind where
  pretty = \case
    KType -> "*"
    ((x :->: y) :->: z) -> parens (pretty $ x :->: y) <+> "→" <+> pretty z
    x :->: y -> pretty x <+> "→" <+> pretty y
    KVar a -> pretty a
    KConstraint -> "Constraint"
