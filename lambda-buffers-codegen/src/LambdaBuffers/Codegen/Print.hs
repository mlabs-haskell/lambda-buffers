module LambdaBuffers.Codegen.Print (
  runPrint,
  Context (..),
  MonadPrint,
  ctxTyImports,
  ctxOpaqueTyImports,
  ctxTyExports,
  ctxClassImports,
  ctxRuleImports,
  ctxConfig,
  ctxCompilerInput,
  ctxModule,
  importValue,
  importClass,
  stValueImports,
  stClassImports,
  stTypeImports,
  throwInternalError,
  importType,
  throwInternalError',
) where

import Control.Lens (makeLenses, (&), (.~))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (Except, runExcept)
import Control.Monad.RWS (RWST (runRWST))
import Control.Monad.RWS.Class (MonadRWS, modify)
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

data Context qtn qcn = Context
  { _ctxCompilerInput :: PC.CodegenInput
  , _ctxModule :: PC.Module -- TODO(bladyjoker): Turn into a `ModuleName` and do a lookup on the CI.
  , _ctxTyImports :: Set PC.QTyName
  , _ctxOpaqueTyImports :: Set qtn
  , _ctxClassImports :: Set qcn
  , _ctxRuleImports :: Set (PC.InfoLess PC.ModuleName)
  , _ctxTyExports :: Set (PC.InfoLess PC.TyName)
  , _ctxConfig :: Config qtn qcn
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'Context

data State qcn qvn qtn = State
  { _stValueImports :: Set qvn
  , _stClassImports :: Set qcn
  , _stTypeImports :: Set qtn
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'State

type MonadPrint qtn qcn qvn m = (MonadError Error m, MonadRWS (Context qtn qcn) () (State qcn qvn qtn) m)

type PrintM qtn qcn qvn = RWST (Context qtn qcn) () (State qcn qvn qtn) (Except Error)

-- | `runPrint ctx printer` runs a printing workflow that yields a module document and a set of package dependencies.
runPrint ::
  forall qtn qcn qvn.
  (Ord qvn, Ord qcn, Ord qtn) =>
  Context qtn qcn ->
  PrintM qtn qcn qvn (Doc (), Set Text) ->
  Either P.Error (Doc (), Set Text)
runPrint ctx modPrinter =
  let p = runRWST modPrinter ctx (State mempty mempty mempty)
   in case runExcept p of
        Left err ->
          Left $
            defMessage
              & P.internalErrors
                .~ [ err
                   ]
        Right (r, _, _) -> Right r

importValue :: (MonadPrint qtn qcn qvn m, Ord qvn) => qvn -> m ()
importValue qvn = modify (\(State vimps cimps tyimps) -> State (Set.insert qvn vimps) cimps tyimps)

importClass :: (MonadPrint qtn qcn qvn m, Ord qcn) => qcn -> m ()
importClass qcn = modify (\(State vimps cimps tyimps) -> State vimps (Set.insert qcn cimps) tyimps)

importType :: (MonadPrint qtn qcn qvn m, Ord qtn) => qtn -> m ()
importType qtn = modify (\(State vimps cimps tyimps) -> State vimps cimps (Set.insert qtn tyimps))

throwInternalError :: MonadPrint qtn qcn qvn m => PC.SourceInfo -> String -> m a
throwInternalError si = throwInternalError' si . Text.pack

throwInternalError' :: MonadPrint qtn qcn qvn m => PC.SourceInfo -> Text -> m a
throwInternalError' si msg =
  throwError $
    defMessage
      & P.msg .~ "[LambdaBuffers.Codegen.Print] " <> msg
      & P.sourceInfo .~ PC.toProto si
