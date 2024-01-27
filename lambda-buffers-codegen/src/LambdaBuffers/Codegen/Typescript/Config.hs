{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Codegen.Typescript.Config (Config) where

import Data.Aeson (FromJSON, ToJSON)
import LambdaBuffers.Codegen.Config qualified as Config
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts

type Config = Config.Config Ts.QTyName Ts.QClassName

instance ToJSON Ts.TyName
instance FromJSON Ts.TyName

instance ToJSON Ts.ClassName
instance FromJSON Ts.ClassName

instance ToJSON Ts.ModuleName
instance FromJSON Ts.ModuleName

instance ToJSON Ts.PackageName
instance FromJSON Ts.PackageName
