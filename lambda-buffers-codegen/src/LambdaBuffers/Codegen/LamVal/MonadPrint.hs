module LambdaBuffers.Codegen.LamVal.MonadPrint (MonadPrint, runPrint, freshArg, resolveRef, importValue) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (Except, runExcept)
import Control.Monad.RWS (RWST (runRWST))
import Control.Monad.RWS.Class (MonadRWS, asks, gets, modify)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Codegen.LamVal (Ref, ValueE (VarE), ValueName)
import Prettyprinter (Doc)

newtype PrintRead qvn = MkPrintRead
  { builtins :: Map ValueName qvn
  }
  deriving stock (Show)

data PrintState qvn = MkPrintState
  { currentVar :: Int
  , valueImports :: Set qvn
  }
  deriving stock (Eq, Ord, Show)

type PrintError = String

type MonadPrint m qvn = (MonadRWS (PrintRead qvn) () (PrintState qvn) m, MonadError PrintError m)

type PrintM qvn = RWST (PrintRead qvn) () (PrintState qvn) (Except PrintError)

runPrint :: Ord qvn => Map ValueName qvn -> PrintM qvn (Doc ann) -> Either PrintError (Doc ann, Set qvn)
runPrint lamValBuiltins printer =
  let p = runExcept $ runRWST printer (MkPrintRead lamValBuiltins) (MkPrintState 0 mempty)
   in case p of
        Left err -> Left err
        Right (doc, st, _) -> Right (doc, valueImports st)

freshArg :: MonadPrint m qvn => m ValueE
freshArg = do
  i <- gets currentVar
  modify (\(MkPrintState curr imps) -> MkPrintState (curr + 1) imps)
  return $ VarE $ "x" <> show i

importValue :: (MonadPrint m qvn, Ord qvn) => qvn -> m qvn
importValue qvn = modify (\(MkPrintState curr imps) -> MkPrintState curr (Set.insert qvn imps)) >> return qvn

{- | Resolves a `Ref` to a value reference in the target language.
 Resolves a `LV.Ref` which is a reference to a LamVal 'builtin', to the equivalent in the target language.
 If the `LV.Ref` is polymorphic like `eq @Int` or `eq @(Maybe a)` or `eq @(List Int)`,
 then lookup the implementation in the context and error if it's not there (TODO).

 NOTE(bladyjoker): Currently, this is assuming all the implementations are imported.
 TODO(bladyjoker): Output all necessary implementations from the Compiler and report on missing.
-}
resolveRef :: MonadPrint m qvn => Ref -> m qvn
resolveRef (_, refName) = do
  bs <- asks builtins
  case Map.lookup refName bs of
    Nothing -> throwError $ "TODO(bladyjoker): LamVal builtin mapping for " <> show refName <> " not configured."
    Just qvn -> return qvn
