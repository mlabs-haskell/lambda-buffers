module Test.LambdaBuffers.Codegen (tests) where

import Test.LambdaBuffers.Codegen.Haskell qualified as H
import Test.LambdaBuffers.Codegen.Plutarch qualified as Plutarch
import Test.LambdaBuffers.Codegen.Purescript qualified as Purs
import Test.LambdaBuffers.Codegen.Rust qualified as Rust
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "LambdaBuffers.Codegen"
    [H.tests, Purs.tests, Plutarch.tests, Rust.tests]
