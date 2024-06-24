{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.LambdaBuffers.Plutarch.Golden (readGoldenPdJson) where

import Data.ByteString qualified as B
import LambdaBuffers.Runtime.Plutarch ()
import LambdaBuffers.Runtime.Plutus ()
import LambdaBuffers.Runtime.Prelude.Json qualified as Lb
import Paths_lbt_plutus_golden_data qualified as Paths
import PlutusTx.IsData (FromData, fromData)
import System.Exit (exitFailure)
import System.FilePath ((</>))

readPdJson :: FromData b => FilePath -> IO b
readPdJson fp = do
  content <- B.readFile fp
  case Lb.fromJsonBytes content of
    Left err -> do
      print ("Error while parsing LambdaBuffers .pd.json file" :: String, fp, err)
      exitFailure
    Right pd -> do
      case fromData pd of
        Nothing -> do
          print ("Error while parsing LambdaBuffers PlutusData" :: String, fp)
          exitFailure
        Just x -> return x

readGoldenPdJson :: FromData b => FilePath -> IO b
readGoldenPdJson fp = do
  dataDir <- Paths.getDataDir
  readPdJson (dataDir </> "data" </> fp)
