module Main (main) where

import Test.LambdaBuffers.Runtime.Plutus.Json qualified as PlutusJson
import Test.LambdaBuffers.Runtime.Plutus.PlutusData qualified as PlutusPd
import Test.LambdaBuffers.Runtime.Plutus.PlutusTx qualified as PlutusTx
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  plutusDataTests <- PlutusPd.tests
  jsonTests <- PlutusJson.tests
  defaultMain $
    testGroup
      "LambdaBuffers Plutus package tests"
      [ plutusDataTests
      , jsonTests
      , PlutusTx.tests
      ]
