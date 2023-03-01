module LambdaBuffers.Codegen.Haskell.Config (QTyName, QClassName, Config (..), opaques, classes) where

import Control.Lens (makeLenses)
import Data.Map (Map)
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

type QTyName = (PC.InfoLess PC.ModuleName, PC.InfoLess PC.TyName)
type QClassName = (PC.InfoLess PC.ModuleName, PC.InfoLess PC.ClassName)

data Config = MkConfig
  { _opaques :: Map QTyName (H.CabalPackageName, H.ModuleName, H.TyName)
  , _classes :: Map QClassName (H.CabalPackageName, H.ModuleName, H.ClassName, [H.FunctionName])
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'MkConfig
