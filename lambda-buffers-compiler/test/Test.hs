module Main (main) where

import Test.KindCheck qualified as KC
import Test.Tasty (defaultMain, testGroup)

-- import Test.TypeClassCheck qualified as TC

main :: IO ()
main =
  defaultMain $
    testGroup
      "Compiler tests"
      [ KC.test
      -- , TC.test
      ]
