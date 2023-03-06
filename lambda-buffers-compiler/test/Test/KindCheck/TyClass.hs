module Test.KindCheck.TyClass (test) where

import LambdaBuffers.Compiler.KindCheck (check_)
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types (CompilerInput)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Providers (TestName)
import Test.Utils.CompilerInput (compilerInput'classEq, compilerInput'classOrd)
import Test.Utils.Constructors ()
import Test.Utils.TyDef ()

test :: TestTree
test = testGroup "KC Class definitions Error Group." [ok'Eq, ok'Ord]

ok'Eq :: TestTree
ok'Eq = oKCase "Class Eq definition." compilerInput'classEq

ok'Ord :: TestTree
ok'Ord = oKCase "Class Ord definition." compilerInput'classOrd

oKCase :: TestName -> CompilerInput -> TestTree
oKCase n x = testCase n $ oK x
  where
    oK :: CompilerInput -> Assertion
    oK a = check_ a @?= Right ()
