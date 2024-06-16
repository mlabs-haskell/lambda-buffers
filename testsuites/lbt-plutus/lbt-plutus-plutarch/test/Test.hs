module Main (main) where

import Test.LambdaBuffers.Runtime.Plutarch.PlutusData qualified as PlutusData
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "LambdaBuffers Plutarch support tests (for Plutus package but also Prelude)"
      [ PlutusData.tests
      ]
