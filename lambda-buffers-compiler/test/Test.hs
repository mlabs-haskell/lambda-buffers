module Main (main) where

import Test.DeriveCheck qualified as DC
import Test.KindCheck qualified as KC
import Test.LambdaBuffers.Compiler qualified as LBC
import Test.LambdaBuffers.Compiler.MiniLog qualified as ML
import Test.LambdaBuffers.Compiler.ProtoCompat.Eval qualified as E
import Test.LambdaBuffers.Compiler.TypeClassCheck qualified as TC2
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
      , ML.test
      , TC2.test
      ]
