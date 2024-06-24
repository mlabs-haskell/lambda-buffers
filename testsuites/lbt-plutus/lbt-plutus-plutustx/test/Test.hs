module Main (main) where

import Test.LambdaBuffers.Runtime.PlutusTx.PlutusData qualified as PlutusData
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "LambdaBuffers Plutus package tests"
      [ PlutusData.tests
      ]
