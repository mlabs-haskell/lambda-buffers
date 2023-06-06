module Main (main) where

import Test.LambdaBuffers.Runtime.Json.Prelude qualified as JsonPrelude
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "LambdaBuffers Json runtime tests"
      [ JsonPrelude.test
      ]
