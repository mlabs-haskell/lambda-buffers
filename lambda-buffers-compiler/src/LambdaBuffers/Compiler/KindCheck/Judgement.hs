{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module LambdaBuffers.Compiler.KindCheck.Judgement (Judgement (Judgement), j'type, j'kind, j'ctx) where

import Control.Lens (makeLenses, (^.))
import LambdaBuffers.Compiler.KindCheck.Context (Context)
import LambdaBuffers.Compiler.KindCheck.Kind (Kind)
import LambdaBuffers.Compiler.KindCheck.Type (Type)
import Prettyprinter (Pretty (pretty), (<+>))

data Judgement = Judgement
  { _j'ctx :: Context
  , _j'type :: Type
  , _j'kind :: Kind
  }
  deriving stock (Show, Eq)
makeLenses ''Judgement

instance Pretty Judgement where
  pretty j = pretty (j ^. j'ctx) <+> "⊢" <+> pretty (j ^. j'type) <+> ":" <+> pretty (j ^. j'kind)
