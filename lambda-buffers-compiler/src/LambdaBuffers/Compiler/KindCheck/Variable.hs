module LambdaBuffers.Compiler.KindCheck.Variable (Variable (LocalRef, ForeignRef, TyVar), Atom) where

import Data.Text (Text)
import GHC.Generics (Generic)
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import Prettyprinter (Pretty (pretty), viaShow)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary (GenericArbitrary))
import Test.QuickCheck.Instances.Text ()

type Atom = Text

data Variable
  = LocalRef PC.LocalRef
  | ForeignRef PC.ForeignRef
  | TyVar PC.VarName
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericArbitrary Variable

instance Pretty Variable where
  pretty = viaShow

-- LocalRef a -> pretty a
-- ForeignRef ms a -> concatWith (\x y -> x <> "." <> y) (pretty <$> (ms <> [a]))
