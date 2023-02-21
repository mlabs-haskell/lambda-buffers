module Test.LambdaBuffers.Compiler.Gen.Mutation (shuffleModules, shuffleTyDefs, Mutation (..)) where

import Control.Lens ((&), (.~), (^.))
import Data.List.NonEmpty (nonEmpty)
import Data.ProtoLens (Message (messageName))
import Data.Proxy (Proxy (Proxy))
import GHC.Stack (HasCallStack)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H
import Proto.Compiler (CompilerError, CompilerInput, CompilerOutput, CompilerOutput'CompilerOutput (CompilerOutput'CompilerError, CompilerOutput'CompilerResult), CompilerResult)
import Proto.Compiler_Fields (maybe'compilerOutput, modules, typeDefs)
import Test.LambdaBuffers.Compiler.Gen.Utils (pick)
import Test.Tasty (TestName)

data Mutation m = MkMutation
  { mutLabel :: TestName
  , mutFn :: CompilerInput -> H.Gen CompilerInput
  , mutAssert :: H.MonadTest m => CompilerOutput -> m ()
  }

instance Show (Mutation m) where
  show :: Mutation m -> String
  show = show . mutLabel

-- | Benign mutations
shuffleModules :: Mutation m
shuffleModules =
  MkMutation
    "Shuffling modules inside of the CompilerInput should not affect compilation"
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
compilerOut :: HasCallStack => H.MonadTest m => (CompilerError -> m ()) -> (CompilerResult -> m ()) -> CompilerOutput -> m ()
compilerOut err res co = do
  H.annotate "Received CompilerOutput"
  H.annotateShow co
  case co ^. maybe'compilerOutput of
    Nothing -> do
      H.annotate $ "compiler_output field must be set in " <> show (messageName (Proxy @CompilerOutput))
      H.failure
    Just (CompilerOutput'CompilerError cerr) -> err cerr
    Just (CompilerOutput'CompilerResult cres) -> res cres

compilerResOrFail :: HasCallStack => H.MonadTest m => (CompilerResult -> m ()) -> CompilerOutput -> m ()
compilerResOrFail =
  compilerOut
    ( \cerr -> do
        H.annotateShow cerr
        H.failure
    )
