module Main (main) where

import Test.LambdaBuffers.Runtime.Prelude.Eq qualified as PreludeEq
import Test.LambdaBuffers.Runtime.Prelude.Json qualified as PreludeJson
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  jsonTests <- PreludeJson.tests
  defaultMain $
    testGroup
      "LambdaBuffers `lbf-prelude` package runtime tests"
      [ jsonTests
      , PreludeEq.tests
      ]
