module LambdaBuffers.Compiler.KindCheck.Judgement (Judgement (Judgement), getJudgement, jType, jKind) where

import Control.Lens (Lens', lens)
import LambdaBuffers.Compiler.KindCheck.Context (Context)
import LambdaBuffers.Compiler.KindCheck.Kind (Kind)
import LambdaBuffers.Compiler.KindCheck.Type (Type)
import Prettyprinter (Pretty (pretty), (<+>))

newtype Judgement = Judgement {getJudgement :: (Context, Type, Kind)}
  deriving stock (Show, Eq)

jType :: Lens' Judgement Type
jType = lens from to
  where
    from = (\(_, x, _) -> x) . getJudgement
    to (Judgement (c, _, k)) t = Judgement (c, t, k)

jKind :: Lens' Judgement Kind
jKind = lens from to
  where
    from = (\(_, _, k) -> k) . getJudgement
    to (Judgement (c, t, _)) k = Judgement (c, t, k)

instance Pretty Judgement where
  pretty (Judgement (c, t, k)) = pretty c <> " ⊢ " <> pretty t <+> ":" <+> pretty k
