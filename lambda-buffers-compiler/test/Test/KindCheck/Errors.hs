module Test.KindCheck.Errors (testGKindCheckErrors) where

import LambdaBuffers.Compiler.KindCheck (check_)
import LambdaBuffers.Compiler.ProtoCompat.Types (CompilerError (CompKindCheckError), KindCheckError (UnboundTyRefError, UnboundTyVarError))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Utils.CompilerInput (compilerInput'undefinedForeignTyRef, compilerInput'undefinedLocalTyRef, compilerInput'undefinedVariable)
import Test.Utils.TyDef (tyDef'undefinedForeignTyRef, tyDef'undefinedForeignTyRef'TyRef, tyDef'undefinedLocalTyRef, tyDef'undefinedLocalTyRef'TyRef, tyDef'undefinedVar, tyDef'undefinedVar'var)

testGKindCheckErrors :: TestTree
testGKindCheckErrors = testGroup "Kind Check Error Tests" [undefinedVariable, undefinedLocalTyRef, undefinedForeignTyRef]

undefinedVariable :: TestTree
undefinedVariable =
  testCase "Catch undefined(free) variable in Type Definition." $
    check_ compilerInput'undefinedVariable @?= Left (CompKindCheckError $ UnboundTyVarError tyDef'undefinedVar tyDef'undefinedVar'var)

undefinedLocalTyRef :: TestTree
undefinedLocalTyRef =
  testCase "Catch undefined Local TyRef in Type Definition." $
    check_ compilerInput'undefinedLocalTyRef @?= Left (CompKindCheckError $ UnboundTyRefError tyDef'undefinedLocalTyRef tyDef'undefinedLocalTyRef'TyRef)

undefinedForeignTyRef :: TestTree
undefinedForeignTyRef =
  testCase "Catch undefined Foreign TyRef in Type Definition." $
    check_ compilerInput'undefinedForeignTyRef @?= Left (CompKindCheckError $ UnboundTyRefError tyDef'undefinedForeignTyRef tyDef'undefinedForeignTyRef'TyRef)
