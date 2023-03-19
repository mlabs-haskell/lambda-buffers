{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Codegen.Haskell.Config (Config) where

import Data.Aeson (FromJSON, ToJSON)
import LambdaBuffers.Codegen.Config qualified as Config
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H

type Config = Config.Config H.QTyName [H.QClassName]

instance ToJSON H.TyName
instance FromJSON H.TyName

instance ToJSON H.ClassName
instance FromJSON H.ClassName

instance ToJSON H.ModuleName
instance FromJSON H.ModuleName

instance ToJSON H.CabalPackageName
instance FromJSON H.CabalPackageName
