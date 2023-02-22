module LambdaBuffers.Compiler.KindCheck.Derivation (
  Derivation (Axiom, Abstraction, Application, Implication),
  dType,
  dTopKind,
) where

import Control.Lens (Lens', lens, (&), (.~), (^.))
import LambdaBuffers.Compiler.KindCheck.Judgement (Judgement, jKind, jType)
import LambdaBuffers.Compiler.KindCheck.Kind (Kind)
import LambdaBuffers.Compiler.KindCheck.Type (Type)
import Prettyprinter (
  Doc,
  Pretty (pretty),
  encloseSep,
  hang,
  lbracket,
  line,
  rbracket,
  space,
 )

data Derivation
  = Axiom Judgement
  | Abstraction Judgement Derivation
  | Application Judgement Derivation Derivation
  | Implication Judgement Derivation
  deriving stock (Show, Eq)

instance Pretty Derivation where
  pretty x = case x of
    Axiom j -> hang 2 $ pretty j
    Abstraction j d -> dNest j [d]
    Application j d1 d2 -> dNest j [d1, d2]
    Implication j d -> dNest j [d]
    where
      dNest :: forall a b c. (Pretty a, Pretty b) => a -> [b] -> Doc c
      dNest j ds = pretty j <> line <> hang 2 (encloseSep (lbracket <> space) rbracket (space <> "∧" <> space) (pretty <$> ds))

dType :: Lens' Derivation Type
dType = lens from to
  where
    from = \case
      Axiom j -> j ^. jType
      Abstraction j _ -> j ^. jType
      Application j _ _ -> j ^. jType
      Implication j _ -> j ^. jType

    to drv t = case drv of
      Axiom j -> Axiom $ j & jType .~ t
      Abstraction j d -> Abstraction (j & jType .~ t) d
      Application j d1 d2 -> Application (j & jType .~ t) d1 d2
      Implication j d -> Implication (j & jType .~ t) d

dTopKind :: Lens' Derivation Kind
dTopKind = lens from to
  where
    from = \case
      Axiom j -> j ^. jKind
      Abstraction j _ -> j ^. jKind
      Application j _ _ -> j ^. jKind
      Implication j _ -> j ^. jKind

    to der t = case der of
      Axiom j -> Axiom $ j & jKind .~ t
      Abstraction j d -> Abstraction (j & jKind .~ t) d
      Application j d1 d2 -> Application (j & jKind .~ t) d1 d2
      Implication j d -> Abstraction (j & jKind .~ t) d
