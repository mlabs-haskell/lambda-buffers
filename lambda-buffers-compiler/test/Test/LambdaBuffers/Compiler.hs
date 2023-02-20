module Test.LambdaBuffers.Compiler (test) where

import Control.Lens ((&), (.~))
import Data.ProtoLens (Message (defMessage))
import LambdaBuffers.Compiler (runCompiler)
import Proto.Compiler (CompilerOutput)
import Proto.Compiler_Fields (compilerResult)
import Test.LambdaBuffers.Compiler.Gen (genCompilerInput)
import Test.LambdaBuffers.Compiler.Gen.Mutation qualified as Mut
import Test.QuickCheck (forAll, forAllBlind)
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

test :: TestTree
test =
  testGroup
    "Compiler Proto API tests"
    [ allCorrectCompInpCompile
    , allCorrectCompInpCompileAfterBenignMut
    ]

compilationOk :: CompilerOutput -> Bool
compilationOk compOut = compOut == (defMessage & compilerResult .~ defMessage)

allCorrectCompInpCompile :: TestTree
allCorrectCompInpCompile = testProperty "All correct CompilerInputs must compile" (forAll genCompilerInput (compilationOk . runCompiler))

allCorrectCompInpCompileAfterBenignMut :: TestTree
allCorrectCompInpCompileAfterBenignMut =
  testProperty
    "All correct CompilerInputs must compile after a benign mutation"
    $ forAll
      genCompilerInput
      ( \compInp ->
          forAll
            ( QC.elements
                [ Mut.shuffleModules
                , Mut.shuffleTyDefs
                ]
            )
            ( \mut ->
                forAllBlind
                  (Mut.mutFn mut compInp)
                  ( \(compInp', _) ->
                      let compOut = runCompiler compInp
                          compOut' = runCompiler compInp'
                       in compilationOk compOut && compilationOk compOut'
                  )
            )
      )

-- TODO(bladyjoker): Add error producing mutations.
