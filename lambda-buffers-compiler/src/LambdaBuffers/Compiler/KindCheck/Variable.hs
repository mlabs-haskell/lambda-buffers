module LambdaBuffers.Compiler.KindCheck.Variable () where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.InfoLess (InfoLess, InfoLessC, withInfoLess)
import Prettyprinter (Pretty (pretty), viaShow)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary (GenericArbitrary))

type Atom = Integer
