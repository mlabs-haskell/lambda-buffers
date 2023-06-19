module Main (main) where

import Test.LambdaBuffers.Runtime.Prelude.Eq qualified as PreludeEq
import Test.LambdaBuffers.Runtime.Prelude.Json qualified as PreludeJson
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "LambdaBuffers `lbf-prelude` package runtime tests"
      [ PreludeJson.test
      , PreludeEq.test
      ]
