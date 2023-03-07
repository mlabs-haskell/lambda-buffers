module Test.KindCheck.TyClass (test) where

import LambdaBuffers.Compiler.KindCheck (check_)
import LambdaBuffers.Compiler.ProtoCompat.Types (CompilerInput)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Providers (TestName)
import Test.Utils.CompilerInput (compilerInput'IntEqInstance, compilerInput'classEq, compilerInput'classOrd, compilerInput'unboundEq)
import Test.Utils.Constructors ()
import Test.Utils.TyDef ()

test :: TestTree
test =
  testGroup
    "KC Class Error."
    [ testGroup "Class Defs." [ok'Eq, ok'Ord, fail'Ord]
    , testGroup "Class Instances." [ok'IntEqInstance]
    ]

--------------------------------------------------------------------------------
-- Class Defs

ok'Eq :: TestTree
ok'Eq = oKCase "Class Eq definition." compilerInput'classEq

ok'Ord :: TestTree
ok'Ord = oKCase "Class Ord definition." compilerInput'classOrd

fail'Ord :: TestTree
fail'Ord = failCase "Unbound reference to eq." compilerInput'unboundEq

--------------------------------------------------------------------------------
-- Class Instances

ok'IntEqInstance :: TestTree
ok'IntEqInstance = oKCase "Int Eq Instance." compilerInput'IntEqInstance

--------------------------------------------------------------------------------
-- Utils

failCase :: TestName -> CompilerInput -> TestTree
failCase n x = testCase n $ justFail x
  where
    justFail :: CompilerInput -> Assertion
    justFail a = either (Left . const ()) Right (check_ a) @?= Left ()

oKCase :: TestName -> CompilerInput -> TestTree
oKCase n x = testCase n $ oK x
  where
    oK :: CompilerInput -> Assertion
    oK a = check_ a @?= Right ()
