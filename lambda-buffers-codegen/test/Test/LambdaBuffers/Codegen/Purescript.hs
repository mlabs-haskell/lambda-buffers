module Test.LambdaBuffers.Codegen.Purescript (tests) where

import Data.Aeson qualified as A
import Data.Functor (void)
import LambdaBuffers.Codegen.Purescript.Config qualified as Purs
import Paths_lambda_buffers_codegen qualified as Paths
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "LambdaBuffers.Codegen.Purescript"
    [ configParses
    ]

configParses :: TestTree
configParses = testCase "Purescript config parses" $ do
  configFp <- Paths.getDataFileName "data/purescript.json"
  void $ readPurescriptConfig configFp

readPurescriptConfig :: FilePath -> IO Purs.Config
readPurescriptConfig f = do
  mayCfg <- A.decodeFileStrict f
  case mayCfg of
    Nothing -> error $ "Invalid Purescript configuration file " <> f
    Just cfg -> return cfg
