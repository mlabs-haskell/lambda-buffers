module Test.LambdaBuffers.Codegen.Rust (tests) where

import Data.Aeson qualified as A
import Data.Functor (void)
import LambdaBuffers.Codegen.Rust.Config qualified as R
import Paths_lambda_buffers_codegen qualified as Paths
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "LambdaBuffers.Codegen.Rust"
    [ configParses
    ]

configParses :: TestTree
configParses = testCase "Rust config parses" $ do
  preludeBaseConfigFp <- Paths.getDataFileName "data/rust-prelude-base.json"
  void $ readRustConfig preludeBaseConfigFp
  plutusPlaConfigFp <- Paths.getDataFileName "data/rust-plutus-pla.json"
  void $ readRustConfig plutusPlaConfigFp

readRustConfig :: FilePath -> IO R.Config
readRustConfig f = do
  mayCfg <- A.decodeFileStrict f
  case mayCfg of
    Nothing -> error $ "Invalid Rust configuration file " <> f
    Just cfg -> return cfg
