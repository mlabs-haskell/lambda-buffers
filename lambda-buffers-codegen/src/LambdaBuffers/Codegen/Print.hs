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
) where

import Control.Lens (makeLenses, (&), (.~))
import Control.Monad.Error.Class (MonadError)
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

type Error = (PC.SourceInfo, Text)

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

data State qcn qvn = State
  { _stValueImports :: Set qvn
  , _stClassImports :: Set qcn
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'State

type MonadPrint qtn qcn qvn m = (MonadError Error m, MonadRWS (Context qtn qcn) () (State qcn qvn) m)

type PrintM qtn qcn qvn = RWST (Context qtn qcn) () (State qcn qvn) (Except Error)

runPrint ::
  forall qtn qcn qvn.
  (Ord qvn, Ord qcn) =>
  Context qtn qcn ->
  PrintM qtn qcn qvn (Doc ()) ->
  Either P.Error (Doc ())
runPrint ctx modPrinter =
  let p = runRWST modPrinter ctx (State mempty mempty)
   in case runExcept p of
        Left err ->
          Left $
            defMessage
              & P.internalErrors
                .~ [ defMessage
                      & P.msg .~ (Text.pack . show $ err) -- TODO(bladyjoker): Introduce SourceInfo in InternalError.
                   ]
        Right (doc, _, _) -> Right doc

importValue :: (MonadPrint qtn qcn qvn m, Ord qvn) => qvn -> m ()
importValue qvn = modify (\(State vimps cimps) -> State (Set.insert qvn vimps) cimps)

importClass :: (MonadPrint qtn qcn qvn m, Ord qcn) => qcn -> m ()
importClass qcn = modify (\(State vimps cimps) -> State vimps (Set.insert qcn cimps))
