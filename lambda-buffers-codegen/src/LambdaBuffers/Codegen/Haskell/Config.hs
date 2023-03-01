module LambdaBuffers.Codegen.Haskell.Config (QTyName, QClassName, Config (..), opaques, classes) where

import Control.Lens (makeLenses)
import Data.Map (Map)
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

-- FIXME(bladyjoker): Must be InfoLess.
type QTyName = (PC.ModuleName, PC.TyName)
type QClassName = (PC.ModuleName, PC.ClassName)

data Config = MkConfig
  { _opaques :: Map QTyName (H.CabalPackageName, H.ModuleName, H.TyName)
  , _classes :: Map QClassName (H.CabalPackageName, H.ModuleName, H.ClassName, [H.FunctionName])
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'MkConfig
