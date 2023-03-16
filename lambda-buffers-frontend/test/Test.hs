module Main (main) where

import Paths_lambda_buffers_frontend qualified as Paths
import Test.LambdaBuffers.Frontend (tests)
import Test.LambdaBuffers.Frontend.Parsec qualified as Parsec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  dataDir <- Paths.getDataFileName "data"
  defaultMain $
    testGroup
      "Frontend tests"
      [ tests dataDir
      , Parsec.tests
      ]
