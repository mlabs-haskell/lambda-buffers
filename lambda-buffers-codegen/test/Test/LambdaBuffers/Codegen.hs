module Test.LambdaBuffers.Codegen (tests) where

import Test.LambdaBuffers.Codegen.Haskell qualified as H
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "LambdaBuffers.Codegen"
    [H.tests]
