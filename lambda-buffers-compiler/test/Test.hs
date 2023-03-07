module Main (main) where

import Test.DeriveCheck qualified as DC
import Test.KindCheck qualified as KC
import Test.LambdaBuffers.Compiler qualified as LBC
import Test.LambdaBuffers.Compiler.ProtoCompat.Eval qualified as E
import Test.Tasty (defaultMain, testGroup)
import Test.TypeClassCheck qualified as TC

main :: IO ()
main =
  defaultMain $
    testGroup
      "Compiler tests"
      [ KC.test
      , TC.test
      , DC.test
      , LBC.test
      , E.test
      ]
