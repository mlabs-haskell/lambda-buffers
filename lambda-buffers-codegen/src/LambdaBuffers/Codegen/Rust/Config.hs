{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Codegen.Rust.Config (Config) where

import Data.Aeson (FromJSON, ToJSON)
import LambdaBuffers.Codegen.Config qualified as Config
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R

type Config = Config.Config R.QTyName R.QTraitName

instance ToJSON R.TyName
instance FromJSON R.TyName

instance ToJSON R.ClassName
instance FromJSON R.ClassName

instance ToJSON R.ModuleName
instance FromJSON R.ModuleName

instance ToJSON R.CrateName
instance FromJSON R.CrateName
