{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Codegen.Rust.Config (Config) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as A
import Data.Vector qualified as Vector
import Debug.Trace (trace)
import LambdaBuffers.Codegen.Config qualified as Config
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R

type Config = Config.Config R.QTyName R.QTraitName

instance ToJSON R.TyName
instance FromJSON R.TyName

instance ToJSON R.TraitName
instance FromJSON R.TraitName

instance ToJSON R.ModuleName
instance FromJSON R.ModuleName

instance ToJSON R.CrateName
instance FromJSON R.CrateName

instance (FromJSON a) => FromJSON (R.Qualified a) where
  parseJSON v = do
    A.withArray
      "Qualified"
      ( \arr -> do
          case Vector.toList arr of
            [] -> fail "Cannot parse qualified entity, empty list is invalid"
            [a] -> R.Qualified'Builtin <$> parseJSON a
            cn : parts -> do
              modules <- traverse parseJSON (init (trace (show parts) parts))
              R.Qualified'LibRef
                <$> parseJSON cn
                <*> pure (map R.MkModuleName modules)
                <*> parseJSON (last parts)
      )
      v

instance (ToJSON a) => ToJSON (R.Qualified a) where
  toJSON (R.Qualified'LibRef cn mn a) = A.Array $ Vector.fromList [toJSON cn, toJSON mn, toJSON a]
  toJSON (R.Qualified'Builtin a) = A.Array $ Vector.fromList [toJSON a]
