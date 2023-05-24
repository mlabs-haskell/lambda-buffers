module Test.LambdaBuffers.Compiler.Mutation (shuffleModules, shuffleTyDefs, Mutation (..)) where

import Control.Lens ((&), (.~), (^.))
import Data.List.NonEmpty (nonEmpty)
import Data.ProtoLens (Message (messageName))
import Data.Proxy (Proxy (Proxy))
import GHC.Stack (HasCallStack)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H
import Proto.Compiler (Error, Input, Output, Output'Output (Output'Error, Output'Result), Result)
import Proto.Compiler_Fields (maybe'output, modules)
import Proto.Lang_Fields (typeDefs)
import Test.LambdaBuffers.Compiler.Utils (pick)
import Test.Tasty (TestName)

data Mutation m = MkMutation
  { mutLabel :: TestName
  , mutFn :: Input -> H.Gen Input
  , mutAssert :: H.MonadTest m => Output -> m ()
  }

instance Show (Mutation m) where
  show :: Mutation m -> String
  show = show . mutLabel

-- | Benign mutations
shuffleModules :: Mutation m
shuffleModules =
  MkMutation
    "Shuffling modules inside of the Input should not affect compilation"
    ( \compInp -> do
        shuffled <- H.shuffle (compInp ^. modules)
        return $ compInp & modules .~ shuffled
    )
    (compilerResOrFail (const H.success))

shuffleTyDefs :: Mutation m
shuffleTyDefs =
  MkMutation
    "Shuffling type definitions inside of the Module should not affect compilation"
    ( \compInp -> do
        case nonEmpty $ compInp ^. modules of
          Nothing ->
            return compInp
          Just ms -> do
            (m, ms') <- pick ms
            shuffled <- H.shuffle (m ^. typeDefs)
            let m' = m & typeDefs .~ shuffled
            return $ compInp & modules .~ m' : ms'
    )
    (compilerResOrFail (const H.success))

-- | Utils
compilerOut :: HasCallStack => H.MonadTest m => (Error -> m ()) -> (Result -> m ()) -> Output -> m ()
compilerOut err res co = do
  H.annotate "Received Output"
  H.annotateShow co
  case co ^. maybe'output of
    Nothing -> do
      H.annotate $ "compiler_output field must be set in " <> show (messageName (Proxy @Output))
      H.failure
    Just (Output'Error cerr) -> err cerr
    Just (Output'Result cres) -> res cres

compilerResOrFail :: HasCallStack => H.MonadTest m => (Result -> m ()) -> Output -> m ()
compilerResOrFail =
  compilerOut
    ( \cerr -> do
        H.annotateShow cerr
        H.failure
    )
