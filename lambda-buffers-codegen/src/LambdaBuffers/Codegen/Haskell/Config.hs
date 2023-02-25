module LambdaBuffers.Codegen.Haskell.Config (Config (..), opaques, classes) where

import Control.Lens (makeLenses)
import Data.Map (Map)
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

data Config = MkConfig
  { _opaques :: Map PC.TyName (H.CabalPackageName, H.ModuleName, H.TyName)
  , _classes :: Map (PC.ModuleName, PC.ClassName) (H.CabalPackageName, H.ModuleName, H.ClassName, [H.FunctionName])
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'MkConfig
