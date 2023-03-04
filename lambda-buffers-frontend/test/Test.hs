module Main (main) where

import Paths_lambda_buffers_frontend qualified as Paths
import Test.LambdaBuffers.Frontend (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  extraSrcDir <- Paths.getDataFileName "extra-source-files"
  defaultMain $ testGroup "Frontend tests" [tests extraSrcDir]
