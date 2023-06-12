module Main (main) where

import Test.LambdaBuffers.Runtime.Plutus.Json qualified as PlutusJson
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "LambdaBuffers `lbf-plutus` package runtime tests"
      [ PlutusJson.test
      ]
