module LambdaBuffers.Compiler.KindCheck.Judgement (Judgement (Judgement), getJudgement) where

import LambdaBuffers.Compiler.KindCheck.Context (Context)
import LambdaBuffers.Compiler.KindCheck.Kind (Kind)
import LambdaBuffers.Compiler.KindCheck.Type (Type)
import Prettyprinter (Pretty (pretty), (<+>))

newtype Judgement = Judgement {getJudgement :: (Context, Type, Kind)}
  deriving stock (Show, Eq)

instance Pretty Judgement where
  pretty (Judgement (c, t, k)) = pretty c <> " ⊢ " <> pretty t <+> ":" <+> pretty k
