module LambdaBuffers.Compiler.KindCheck.Variable (Variable (LocalRef, ForeignRef), Atom) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Orphan.Text ()
import Prettyprinter (Pretty (pretty), concatWith)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary (GenericArbitrary))

type Atom = Text

data Variable
  = LocalRef Text
  | ForeignRef [Text] Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericArbitrary Variable

instance Pretty Variable where
  pretty = \case
    LocalRef a -> pretty a
    ForeignRef ms a -> concatWith (\x y -> x <> "." <> y) (pretty <$> (ms <> [a]))
