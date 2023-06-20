module Main (main) where

import Test.LambdaBuffers.Runtime.Plutus.PlutusData qualified as PlutusPd
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  plutusDataTests <- PlutusPd.tests
  defaultMain $
    testGroup
      "LambdaBuffers `lbf-plutus` package runtime tests"
      [ plutusDataTests
      ]
