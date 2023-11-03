module Main (main) where

import Test.LambdaBuffers.Runtime.Plutarch qualified as Pl
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "LambdaBuffers `lbr-plutarch` tests"
      [ Pl.test
      ]
