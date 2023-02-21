module Test.LambdaBuffers.Compiler.Gen.Mutation (shuffleModules, shuffleTyDefs, Mutation (..)) where

import Control.Lens ((&), (.~), (^.))
import Data.List.NonEmpty (nonEmpty)
import Data.ProtoLens (Message (messageName))
import Data.Proxy (Proxy (Proxy))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H
import Proto.Compiler (CompilerError, CompilerInput, CompilerOutput, CompilerOutput'CompilerOutput (CompilerOutput'CompilerError, CompilerOutput'CompilerResult), CompilerResult)
import Proto.Compiler_Fields (maybe'compilerOutput, modules, typeDefs)
import Test.LambdaBuffers.Compiler.Gen.Utils (pick)
import Test.Tasty (TestName)
import Test.Tasty.HUnit (Assertion, HasCallStack, assertFailure)

data Mutation = MkMutation
  { mutLabel :: TestName
  , mutFn :: CompilerInput -> H.Gen (CompilerInput, CompilerOutput -> Assertion)
  }

instance Show Mutation where
  show :: Mutation -> String
  show = show . mutLabel

-- | Benign mutations
shuffleModules :: Mutation
shuffleModules = MkMutation "Shuffling modules inside of the CompilerInput should not affect compilation" $ \compInp -> do
  shuffled <- H.shuffle (compInp ^. modules)
  return
    ( compInp & modules .~ shuffled
    , compilerResOrFail (\_ -> return ())
    )

shuffleTyDefs :: Mutation
shuffleTyDefs = MkMutation "Shuffling type definitions inside of the Module should not affect compilation" $ \compInp -> do
  case nonEmpty $ compInp ^. modules of
    Nothing ->
      return
        ( compInp
        , compilerResOrFail (\_ -> return ())
        )
    Just ms -> do
      (m, ms') <- pick ms
      shuffled <- H.shuffle (m ^. typeDefs)
      let m' = m & typeDefs .~ shuffled
      return
        ( compInp & modules .~ m' : ms'
        , compilerResOrFail (\_ -> return ())
        )

-- | Utils
compilerOut :: HasCallStack => (CompilerError -> Assertion) -> (CompilerResult -> Assertion) -> CompilerOutput -> Assertion
compilerOut err res co = case co ^. maybe'compilerOutput of
  Nothing -> assertFailure $ "compiler_output field must be set in " <> show (messageName (Proxy @CompilerOutput))
  Just (CompilerOutput'CompilerError cerr) -> err cerr
  Just (CompilerOutput'CompilerResult cres) -> res cres

compilerResOrFail :: HasCallStack => (CompilerResult -> Assertion) -> CompilerOutput -> Assertion
compilerResOrFail = compilerOut (\cerr -> assertFailure $ "Expected to succeed but failed with " <> show cerr)
