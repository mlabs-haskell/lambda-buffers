module LambdaBuffers.Codegen.LamVal.MonadPrint (MonadPrint, PrintError, Context (..), runPrint, freshArg, resolveRef, importValue) where

import Control.Lens ((&), (.~))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (Except, runExcept)
import Control.Monad.RWS (RWST (runRWST))
import Control.Monad.RWS.Class (MonadRWS, asks, gets, modify)
import Data.ProtoLens (Message (defMessage))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import LambdaBuffers.Codegen.LamVal (Ref, ValueE (VarE), ValueName)
import Prettyprinter (Doc)
import Proto.Codegen qualified as P
import Proto.Codegen_Fields qualified as P

data Context qvn backend = Context
  { builtins :: !(Ref -> Maybe qvn)
  , backendCtx :: !backend
  }

data State qvn = State
  { currentVar :: !Int
  , valueImports :: !(Set qvn)
  }
  deriving stock (Eq, Ord, Show)

type PrintError = P.InternalError

throwInternalError :: MonadPrint m qvn backend => String -> m a
throwInternalError msg = throwError $ defMessage & P.msg .~ "[LambdaBuffers.Codegen.LamVal.MonadPrint] " <> Text.pack msg

type MonadPrint m qvn backend = (MonadRWS (Context qvn backend) () (State qvn) m, MonadError PrintError m)

type PrintM qvn backend = RWST (Context qvn backend) () (State qvn) (Except PrintError)

runPrint :: Ord qvn => Context qvn backend -> PrintM qvn backend (Doc ann) -> Either PrintError (Doc ann, Set qvn)
runPrint ctx printer =
  let p = runExcept $ runRWST printer ctx (State 0 mempty)
   in case p of
        Left err -> Left err
        Right (doc, st, _) -> Right (doc, valueImports st)

freshArg :: MonadPrint m qvn backend => m ValueE
freshArg = do
  i <- gets currentVar
  modify (\(State curr imps) -> State (curr + 1) imps)
  return $ VarE $ "x" <> show i

importValue :: (MonadPrint m qvn backend, Ord qvn) => qvn -> m qvn
importValue qvn = modify (\(State curr imps) -> State curr (Set.insert qvn imps)) >> return qvn

{- | Resolves a `Ref` to a value reference in the target language.
 Resolves a `LV.Ref` which is a reference to a LamVal 'builtin', to the equivalent in the target language.
 If the `LV.Ref` is polymorphic like `eq @Int` or `eq @(Maybe a)` or `eq @(List Int)`,
 then lookup the implementation in the context and error if it's not there (TODO).

 NOTE(bladyjoker): Currently, this is assuming all the implementations are imported.
 TODO(bladyjoker): Output all necessary implementations from the Compiler and report on missing.
-}
resolveRef :: MonadPrint m qvn backend => Ref -> m qvn
resolveRef ref = do
  bs <- asks builtins
  case bs ref of
    Nothing -> throwInternalError $ "LamVal builtin mapping for " <> show (snd ref :: ValueName) <> " instantiated with types " <> show (fst ref) <> " not configured."
    Just qvn -> return qvn
