module LambdaBuffers.ProtoCompat.Types.Codegen (CodegenInput (..)) where

import Data.Generics.Labels ()
import Data.Map (Map)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import LambdaBuffers.ProtoCompat.InfoLess (InfoLess)
import LambdaBuffers.ProtoCompat.Types.Lang (Module, ModuleName)

newtype CodegenInput = CodegenInput {modules :: Map (InfoLess ModuleName) Module}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Monoid, Semigroup)
  deriving anyclass (SOP.Generic)
