module LambdaBuffers.Compiler.KindCheck.Variable (Variable (ForeignRef, TyVar), Atom) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.InfoLess (InfoLess, InfoLessC, withInfoLess)
import Prettyprinter (Pretty (pretty), viaShow)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary (GenericArbitrary))
import Test.QuickCheck.Instances.Text ()

type Atom = Text

data Variable
  = -- | Notionally all Refs. are fully qualified. The context determines if
    -- they're local or not.
    ForeignRef PC.ForeignRef
  | TyVar PC.VarName
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericArbitrary Variable
  deriving anyclass (SOP.Generic)

instance Pretty Variable where
  pretty = viaShow

instance InfoLessC Variable

instance Pretty (InfoLess Variable) where
  pretty x = withInfoLess x pretty

-- LocalRef a -> pretty a
-- ForeignRef ms a -> concatWith (\x y -> x <> "." <> y) (pretty <$> (ms <> [a]))
