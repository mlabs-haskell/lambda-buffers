module LambdaBuffers.Compiler.KindCheck.Context where

import Control.Lens hiding (Context)
import Data.Bifunctor
import Data.Map qualified as M
import LambdaBuffers.Compiler.KindCheck.Atom
import LambdaBuffers.Compiler.KindCheck.Kind
import Prettyprinter

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

class IsMap a b c where
  toMap :: a -> M.Map b c
  fromMap :: M.Map b c -> a

data ContextType = L | G
  deriving (Eq, Ord, Show)

-- | Utility to unify the two.
getAllContext :: Context -> M.Map Atom Kind
getAllContext c = c ^. context <> c ^. addContext
