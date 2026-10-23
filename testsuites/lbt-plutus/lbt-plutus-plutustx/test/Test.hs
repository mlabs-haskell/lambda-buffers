module Main (main) where

import Test.LambdaBuffers.Runtime.PlutusTx.Eq qualified as PlutusTxEq
import Test.LambdaBuffers.Runtime.PlutusTx.PlutusData qualified as PlutusData
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "LambdaBuffers Plutus package tests"
      [ PlutusTxEq.tests
      , PlutusData.tests
      ]
