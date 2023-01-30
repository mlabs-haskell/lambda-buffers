module LambdaBuffers.Compiler.KindCheck.Context (Context (Context), context, addContext, getAllContext) where

import Control.Lens (makeLenses, (^.))
import Data.Map qualified as M
import LambdaBuffers.Compiler.KindCheck.Kind (Kind)
import LambdaBuffers.Compiler.KindCheck.Variable (Atom)
import Prettyprinter (
  Doc,
  Pretty (pretty),
  braces,
  comma,
  hsep,
  punctuate,
  (<+>),
 )

data Context = Context
  { _context :: M.Map Atom Kind
  , _addContext :: M.Map Atom Kind
  }
  deriving stock (Show, Eq)

makeLenses ''Context

instance Pretty Context where
  pretty c = case M.toList (c ^. addContext) of
    [] -> "Γ"
    ctx -> "Γ" <+> "∪" <+> braces (setPretty ctx)
    where
      setPretty :: [(Atom, Kind)] -> Doc ann
      setPretty = hsep . punctuate comma . fmap (\(v, t) -> pretty v <> ":" <+> pretty t)

instance Semigroup Context where
  (Context a1 b1) <> (Context a2 b2) = Context (a1 <> a2) (b1 <> b2)

instance Monoid Context where
  mempty = Context mempty mempty

-- | Utility to unify the two.
getAllContext :: Context -> M.Map Atom Kind
getAllContext c = c ^. context <> c ^. addContext
