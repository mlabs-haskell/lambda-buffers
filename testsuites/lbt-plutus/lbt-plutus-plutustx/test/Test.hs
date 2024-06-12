module Main (main) where

import Test.LambdaBuffers.Runtime.PlutusTx.PlutusTx qualified as PlutusTx
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "LambdaBuffers Plutus package tests"
      [ PlutusTx.tests
      ]
