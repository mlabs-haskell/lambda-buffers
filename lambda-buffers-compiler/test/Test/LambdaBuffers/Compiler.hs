module Test.LambdaBuffers.Compiler (test) where

import Data.ProtoLens (Message (defMessage))
import LambdaBuffers.Compiler (runCompiler)
import Test.LambdaBuffers.Compiler.Gen (genCompilerInput)
import Test.QuickCheck (forAll, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

test :: TestTree
test =
  testGroup
    "Compiler Proto API tests"
    [ allCorrectCompInpCompile
    ]

allCorrectCompInpCompile :: TestTree
allCorrectCompInpCompile = testProperty "All correct CompilerInputs must compile" (forAll genCompilerInput (\compInp -> runCompiler compInp === Right defMessage))

-- TODO(bladyjoker): Add error producing mutations.
-- TODO(bladyjoker): Add bening mutations (module, tydef, classdef shuffle).
