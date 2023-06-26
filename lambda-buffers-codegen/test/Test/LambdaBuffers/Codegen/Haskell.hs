module Test.LambdaBuffers.Codegen.Haskell (tests) where

import Data.Aeson qualified as A
import Data.Functor (void)
import LambdaBuffers.Codegen.Haskell.Config qualified as H
import Paths_lambda_buffers_codegen qualified as Paths
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "LambdaBuffers.Codegen.Haskell"
    [ configParses
    ]

configParses :: TestTree
configParses = testCase "Haskell config parses" $ do
  configFp <- Paths.getDataFileName "data/haskell.json"
  void $ readHaskellConfig configFp

readHaskellConfig :: FilePath -> IO H.Config
readHaskellConfig f = do
  mayCfg <- A.decodeFileStrict f
  case mayCfg of
    Nothing -> error $ "Invalid Haskell configuration file " <> f
    Just cfg -> return cfg
