{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Codegen.Purescript.Config (Config) where

import Data.Aeson (FromJSON, ToJSON)
import LambdaBuffers.Codegen.Config qualified as Config
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purs

type Config = Config.Config Purs.QTyName [Purs.QClassName]

instance ToJSON Purs.TyName
instance FromJSON Purs.TyName

instance ToJSON Purs.ClassName
instance FromJSON Purs.ClassName

instance ToJSON Purs.ModuleName
instance FromJSON Purs.ModuleName

instance ToJSON Purs.PackageName
instance FromJSON Purs.PackageName
