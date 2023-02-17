module Test.KindCheck.Errors (testGKindCheckErrors) where

import LambdaBuffers.Compiler.KindCheck (check_)
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types (CompilerError (CompKindCheckError), KindCheckError (UnboundTyRefError, UnboundTyVarError), Module)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Utils.CompilerInput (compilerInput'undefinedForeignTyRef, compilerInput'undefinedLocalTyRef, compilerInput'undefinedVariable)
import Test.Utils.Constructors (_ModuleName)
import Test.Utils.TyDef (tyDef'undefinedForeignTyRef, tyDef'undefinedForeignTyRef'TyRef, tyDef'undefinedLocalTyRef, tyDef'undefinedLocalTyRef'TyRef, tyDef'undefinedVar, tyDef'undefinedVar'var)

testGKindCheckErrors :: TestTree
testGKindCheckErrors = testGroup "Kind Check Error Tests" [undefinedVariable, undefinedLocalTyRef, undefinedForeignTyRef]

undefinedVariable :: TestTree
undefinedVariable =
  testCase "Catch undefined(free) variable in Type Definition." $
    check_ compilerInput'undefinedVariable
      @?= (Left . CompKindCheckError . withDefModule) (UnboundTyVarError tyDef'undefinedVar tyDef'undefinedVar'var)

undefinedLocalTyRef :: TestTree
undefinedLocalTyRef =
  testCase "Catch undefined Local TyRef in Type Definition." $
    check_ compilerInput'undefinedLocalTyRef
      @?= (Left . CompKindCheckError . withDefModule) (UnboundTyRefError tyDef'undefinedLocalTyRef tyDef'undefinedLocalTyRef'TyRef)

undefinedForeignTyRef :: TestTree
undefinedForeignTyRef =
  testCase "Catch undefined Foreign TyRef in Type Definition." $
    check_ compilerInput'undefinedForeignTyRef
      @?= (Left . CompKindCheckError . withDefModule) (UnboundTyRefError tyDef'undefinedForeignTyRef tyDef'undefinedForeignTyRef'TyRef)

withDefModule :: forall a. (PC.ModuleName -> a) -> a
withDefModule f = f (_ModuleName ["Module"])
