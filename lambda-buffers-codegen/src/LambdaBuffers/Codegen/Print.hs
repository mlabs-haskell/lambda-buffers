module LambdaBuffers.Codegen.Print (
  runPrint,
  IsBackend (..),
  Context (..),
  MonadPrint,
  PrintM,
  ctxTyImports,
  ctxOpaqueTyImports,
  ctxTyExports,
  ctxClassImports,
  ctxRuleImports,
  ctxConfig,
  ctxCompilerInput,
  ctxModule,
  ctxBackend,
  importValue,
  importClass,
  stValueImports,
  stClassImports,
  stTypeImports,
  stBackend,
  throwInternalError,
  importType,
  throwInternalError',
  askBackend,
  getBackend,
) where

import Control.Lens (makeLenses, (&), (.~))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (Except, runExcept)
import Control.Monad.RWS (RWST (runRWST))
import Control.Monad.RWS.Class (MonadRWS, asks, gets, modify)
import Data.ProtoLens (Message (defMessage))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Config (Config)
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc)
import Proto.Codegen qualified as P
import Proto.Codegen_Fields qualified as P

type Error = P.InternalError

-- TODO(bladyjoker): Add backend error
class
  ( Ord (BackendQualifiedTyName backend)
  , Ord (BackendQualifiedValueName backend)
  , Ord (BackendQualifiedClassName backend)
  ) =>
  IsBackend backend
  where
  type BackendContext backend
  type BackendState backend
  type BackendQualifiedTyName backend
  type BackendQualifiedClassName backend
  type BackendQualifiedValueName backend

data Context backend = Context
  { _ctxCompilerInput :: !PC.CodegenInput
  , _ctxModule :: !PC.Module -- TODO(bladyjoker): Turn into a `ModuleName` and do a lookup on the CI.
  , _ctxTyImports :: !(Set PC.QTyName)
  , _ctxOpaqueTyImports :: !(Set (BackendQualifiedTyName backend))
  , _ctxClassImports :: !(Set (BackendQualifiedClassName backend))
  , _ctxRuleImports :: !(Set (PC.InfoLess PC.ModuleName))
  , _ctxTyExports :: !(Set (PC.InfoLess PC.TyName))
  , _ctxConfig :: !(Config (BackendQualifiedTyName backend) (BackendQualifiedClassName backend))
  , _ctxBackend :: !(BackendContext backend)
  }

makeLenses 'Context

data State backend = State
  { _stValueImports :: Set (BackendQualifiedValueName backend)
  , _stClassImports :: Set (BackendQualifiedClassName backend)
  , _stTypeImports :: Set (BackendQualifiedTyName backend)
  , _stBackend :: BackendState backend
  }

makeLenses 'State

type MonadPrint backend m = (IsBackend backend, MonadError Error m, MonadRWS (Context backend) () (State backend) m)

type PrintM backend = RWST (Context backend) () (State backend) (Except Error)

-- | `runPrint ctx printer` runs a printing workflow that yields a module document and a set of package dependencies.
runPrint ::
  forall backend.
  IsBackend backend =>
  BackendState backend ->
  Context backend ->
  PrintM backend (Doc (), Set Text) ->
  Either P.Error (Doc (), Set Text)
runPrint initSt ctx modPrinter =
  let p = runRWST modPrinter ctx (State mempty mempty mempty initSt)
   in case runExcept p of
        Left err ->
          Left $
            defMessage
              & P.internalErrors
                .~ [ err
                   ]
        Right (r, _, _) -> Right r

importValue :: MonadPrint backend m => BackendQualifiedValueName backend -> m ()
importValue qvn = modify (\(State vimps cimps tyimps bstate) -> State (Set.insert qvn vimps) cimps tyimps bstate)

importClass :: MonadPrint backend m => BackendQualifiedClassName backend -> m ()
importClass qcn = modify (\(State vimps cimps tyimps bstate) -> State vimps (Set.insert qcn cimps) tyimps bstate)

importType :: MonadPrint backend m => BackendQualifiedTyName backend -> m ()
importType qtn = modify (\(State vimps cimps tyimps bstate) -> State vimps cimps (Set.insert qtn tyimps) bstate)

throwInternalError :: MonadPrint backend m => PC.SourceInfo -> String -> m a
throwInternalError si = throwInternalError' si . Text.pack

throwInternalError' :: MonadPrint backend m => PC.SourceInfo -> Text -> m a
throwInternalError' si msg =
  throwError $
    defMessage
      & P.msg .~ "[LambdaBuffers.Codegen.Print] " <> msg
      & P.sourceInfo .~ PC.toProto si

askBackend :: MonadPrint backend m => m (BackendContext backend)
askBackend = asks _ctxBackend

getBackend :: MonadPrint backend m => m (BackendState backend)
getBackend = gets _stBackend
