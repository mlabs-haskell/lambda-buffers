module LambdaBuffers.Compiler.KindCheck.Derivation (
  Derivation (Axiom, Abstraction, Application),
) where

import LambdaBuffers.Compiler.KindCheck.Judgement (Judgement)
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
  deriving stock (Show, Eq)

instance Pretty Derivation where
  pretty x = case x of
    Axiom j -> hang 2 $ pretty j
    Abstraction j d -> dNest j [d]
    Application j d1 d2 -> dNest j [d1, d2]
    where
      dNest :: forall a b c. (Pretty a, Pretty b) => a -> [b] -> Doc c
      dNest j ds = pretty j <> line <> hang 2 (encloseSep (lbracket <> space) rbracket (space <> "∧" <> space) (pretty <$> ds))
