module LambdaBuffers.Compiler.KindCheck.Variable (Variable (LocalRef, ForeignRef), Atom) where

import Data.Text (Text)
import Orphan.Text
import Prettyprinter (Pretty (pretty), concatWith)

type Atom = Text

data Variable
  = LocalRef Text
  | ForeignRef [Text] Text
  deriving stock (Eq, Ord, Show)

instance Pretty Variable where
  pretty = \case
    LocalRef a -> pretty a
    ForeignRef ms a -> concatWith (\x y -> x <> "." <> y) (pretty <$> (ms <> [a]))
