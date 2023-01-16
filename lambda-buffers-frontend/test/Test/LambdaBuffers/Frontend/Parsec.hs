module Test.LambdaBuffers.Frontend.Parsec (tests) where

import Test.Tasty (TestTree, testGroup)

import LambdaBuffers.Frontend.Parsec ()
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "LambdaBuffers.Frontend.Parsec"
    [ testCase "dummy" (True @?= True)
    ]
