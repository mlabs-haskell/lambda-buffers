module LambdaBuffers.Compiler.KindCheck.Kind (Kind (KType, (:->:), KVar), kind2ProtoKind, protoKind2Kind) where

import LambdaBuffers.Compiler.KindCheck.Variable (Atom)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Pretty (pretty), parens, (<+>))

infixr 8 :->:

data Kind
  = KType
  | Kind :->: Kind
  | KVar Atom
  deriving stock (Eq, Show)

instance Pretty Kind where
  pretty = \case
    KType -> "*"
    ((x :->: y) :->: z) -> parens (pretty $ x :->: y) <+> "→" <+> pretty z
    x :->: y -> pretty x <+> "→" <+> pretty y
    KVar a -> pretty a

-- | Convert from internal Kind to Proto Kind.
kind2ProtoKind :: Kind -> PC.Kind
kind2ProtoKind = \case
  k1 :->: k2 -> PC.Kind $ PC.KindArrow (kind2ProtoKind k1) (kind2ProtoKind k2)
  KType -> PC.Kind . PC.KindRef $ PC.KType
  KVar _ -> PC.Kind . PC.KindRef $ PC.KUnspecified -- this shouldn't happen.

-- | Convert from internal Kind to Proto Kind.
protoKind2Kind :: PC.Kind -> Kind
protoKind2Kind = \case
  PC.Kind k -> case k of
    PC.KindArrow k1 k2 -> protoKind2Kind k1 :->: protoKind2Kind k2
    PC.KindRef PC.KType -> KType
    PC.KindRef PC.KUnspecified -> KVar "Unspecified"
