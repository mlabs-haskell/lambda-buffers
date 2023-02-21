module Test.LambdaBuffers.Compiler (test) where

import Control.Lens ((&), (.~))
import Data.ProtoLens (Message (defMessage))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H
import LambdaBuffers.Compiler (runCompiler)
import Proto.Compiler (CompilerOutput)
import Proto.Compiler_Fields (compilerResult)
import Test.LambdaBuffers.Compiler.Gen (genCompilerInput)
import Test.LambdaBuffers.Compiler.Gen.Mutation qualified as Mut
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (HasCallStack)
import Test.Tasty.Hedgehog (testProperty)

test :: TestTree
test =
  testGroup
    "Compiler API tests"
    [ allCorrectCompInpCompile
    , allCorrectCompInpCompileAfterBenignMut
    ]

compilationOk :: H.MonadTest m => CompilerOutput -> m ()
compilationOk compOut = compOut H.=== (defMessage & compilerResult .~ defMessage)

allCorrectCompInpCompile :: HasCallStack => TestTree
allCorrectCompInpCompile = testProperty "All correct CompilerInputs must compile" (H.property $ H.forAll genCompilerInput >>= compilationOk . runCompiler)

allCorrectCompInpCompileAfterBenignMut :: HasCallStack => TestTree
allCorrectCompInpCompileAfterBenignMut =
  testProperty
    "All correct CompilerInputs must compile after a benign mutation"
    $ H.property
    $ do
      compInp <- H.forAll genCompilerInput
      mut <-
        H.forAll $
          H.element
            [ Mut.shuffleModules
            , Mut.shuffleTyDefs
            ]
      compInp' <- H.forAllWith (const "") (Mut.mutFn mut compInp)
      let compOut = runCompiler compInp
          compOut' = runCompiler compInp'
      compilationOk compOut
      compilationOk compOut'
      Mut.mutAssert mut compOut'

-- TODO(bladyjoker): Add error producing mutations.
