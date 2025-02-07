module Test.LambdaBuffers.Compiler (test) where

import Control.Lens ((&), (.~))
import Data.ProtoLens (Message (defMessage))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H
import LambdaBuffers.Compiler (runCompiler)
import Proto.Compiler qualified as Compiler
import Proto.Compiler_Fields qualified as Compiler
import Test.LambdaBuffers.Compiler.Coverage (coverage)
import Test.LambdaBuffers.Compiler.Mutation qualified as Mut
import Test.LambdaBuffers.Compiler.WellFormed (genCompilerInput)
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (HasCallStack)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hedgehog qualified as H

test :: TestTree
test =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "Compiler API tests"
      [ allWellFormedCompInpCompile
      , allWellFormedCompInpCompileAfterBenignMut
      ]

compilationOk :: H.MonadTest m => Compiler.Output -> m ()
compilationOk compOut = compOut H.=== (defMessage & Compiler.result .~ defMessage)

allWellFormedCompInpCompile :: HasCallStack => TestTree
allWellFormedCompInpCompile =
  testProperty
    "All well formed CompilerInputs must compile"
    ( H.property $ do
        compInp <- H.forAll genCompilerInput
        coverage compInp
        compilationOk . runCompiler True $ compInp
    )

allWellFormedCompInpCompileAfterBenignMut :: HasCallStack => TestTree
allWellFormedCompInpCompileAfterBenignMut =
  testProperty
    "All well formed CompilerInputs must compile after a benign mutation"
    $ H.property
    $ do
      compInp <- H.forAll genCompilerInput
      coverage compInp
      mut <-
        H.forAll $
          H.element
            [ Mut.shuffleModules
            , Mut.shuffleTyDefs
            ]
      H.collect mut
      compInp' <- H.forAllWith (const "") (Mut.mutFn mut compInp)
      let compOut = runCompiler True compInp
          compOut' = runCompiler True compInp'
      compilationOk compOut
      compilationOk compOut'
      Mut.mutAssert mut compOut'

-- TODO(bladyjoker): Add error producing mutations.
