module LambdaBuffers.Compiler.KindCheck.Derivation (
  Derivation (Axiom, Abstraction, Application, Implication),
  d'type,
  d'kind,
  Judgement (Judgement),
  j'type,
  j'kind,
  j'ctx,
  Context (Context),
  context,
  addContext,
  getAllContext,
) where

import Control.Lens (Lens', lens, makeLenses, (&), (.~), (^.))
import Data.Map qualified as M
import LambdaBuffers.Compiler.KindCheck.Kind (Kind)
import LambdaBuffers.Compiler.KindCheck.Type (Type, Variable)
import LambdaBuffers.ProtoCompat.InfoLess (InfoLess)
import Prettyprinter (
  Doc,
  Pretty (pretty),
  braces,
  comma,
  encloseSep,
  hang,
  hsep,
  lbracket,
  line,
  punctuate,
  rbracket,
  space,
  (<+>),
 )

data Context = Context
  { _context :: M.Map (InfoLess Variable) Kind
  , _addContext :: M.Map (InfoLess Variable) Kind
  }
  deriving stock (Show, Eq)

makeLenses ''Context

instance Pretty Context where
  pretty c = case M.toList (c ^. addContext) of
    [] -> "Γ"
    ctx -> "Γ" <+> "∪" <+> braces (setPretty ctx)
    where
      setPretty :: [(InfoLess Variable, Kind)] -> Doc ann
      setPretty = hsep . punctuate comma . fmap (\(v, t) -> pretty v <> ":" <+> pretty t)

instance Semigroup Context where
  (Context a1 b1) <> (Context a2 b2) = Context (a1 <> a2) (b1 <> b2)

instance Monoid Context where
  mempty = Context mempty mempty

-- | Utility to unify the two.
getAllContext :: Context -> M.Map (InfoLess Variable) Kind
getAllContext c = c ^. context <> c ^. addContext

data Judgement = Judgement
  { _j'ctx :: Context
  , _j'type :: Type
  , _j'kind :: Kind
  }
  deriving stock (Show, Eq)
makeLenses ''Judgement

instance Pretty Judgement where
  pretty j = pretty (j ^. j'ctx) <+> "⊢" <+> pretty (j ^. j'type) <+> ":" <+> pretty (j ^. j'kind)

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

d'type :: Lens' Derivation Type
d'type = lens from to
  where
    from = \case
      Axiom j -> j ^. j'type
      Abstraction j _ -> j ^. j'type
      Application j _ _ -> j ^. j'type
      Implication j _ -> j ^. j'type

    to drv t = case drv of
      Axiom j -> Axiom $ j & j'type .~ t
      Abstraction j d -> Abstraction (j & j'type .~ t) d
      Application j d1 d2 -> Application (j & j'type .~ t) d1 d2
      Implication j d -> Implication (j & j'type .~ t) d

d'kind :: Lens' Derivation Kind
d'kind = lens from to
  where
    from = \case
      Axiom j -> j ^. j'kind
      Abstraction j _ -> j ^. j'kind
      Application j _ _ -> j ^. j'kind
      Implication j _ -> j ^. j'kind

    to der t = case der of
      Axiom j -> Axiom $ j & j'kind .~ t
      Abstraction j d -> Abstraction (j & j'kind .~ t) d
      Application j d1 d2 -> Application (j & j'kind .~ t) d1 d2
      Implication j d -> Abstraction (j & j'kind .~ t) d
