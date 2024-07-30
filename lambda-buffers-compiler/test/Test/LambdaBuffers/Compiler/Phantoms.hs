module Test.LambdaBuffers.Compiler.Phantoms (test) where

import LambdaBuffers.ProtoCompat qualified as PC
import LambdaBuffers.ProtoCompat.Utils (collectPhantomTyArgs)
import Test.LambdaBuffers.ProtoCompat.Utils qualified as U
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog qualified as H

test :: TestTree
test =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "Phantoms checks"
      [ testCase "phantomA" $ phantoms U.td'phantomA @?= ["a"]
      , testCase "phantomB" $ phantoms U.td'phantomB @?= ["a", "b"]
      , testCase "phantomC" $ phantoms U.td'phantomC @?= ["a"]
      , testCase "phantomD" $ phantoms U.td'phantomD @?= ["a", "b"]
      , testCase "phantomE" $ phantoms U.td'phantomE @?= ["a"]
      , testCase "phantomF" $ phantoms U.td'phantomF @?= ["a", "b"]
      , testCase "Either" $ phantoms U.td'either @?= []
      , testCase "Either opaque" $ phantoms U.td'eitherO @?= []
      , testCase "List" $ phantoms U.td'list @?= []
      , testCase "Maybe" $ phantoms U.td'maybe @?= []
      , testCase "Maybe opaque" $ phantoms U.td'maybeO @?= []
      ]
  where
    phantoms = fmap ((\(PC.VarName name _) -> name) . PC.argName) . collectPhantomTyArgs
