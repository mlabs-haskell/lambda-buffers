module Main (main) where

import Test.LambdaBuffers.Runtime.Json.Plutus qualified as PlutusJson
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "LambdaBuffers Json runtime tests"
      [ PlutusJson.test
      ]
