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
) where

import Control.Lens (makeLenses, (&), (.~))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class (MonadReader)
import Data.ProtoLens (Message (defMessage))
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Config (Config)
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc)
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

type MonadPrint qtn qcn m = (MonadError Error m, MonadReader (Context qtn qcn) m)

type PrintM qtn qcn = ReaderT (Context qtn qcn) (Except Error)

type Error = (PC.SourceInfo, Text)

data Context qtn qcn = MkContext
  { _ctxCompilerInput :: PC.CompilerInput -- TODO(bladyjoker): Use proper `CodegenInput`.
  , _ctxModule :: PC.Module -- TODO(bladyjoker): Turn into a `ModuleName` and do a lookup on the CI.
  , _ctxTyImports :: Set PC.QTyName
  , _ctxOpaqueTyImports :: Set qtn
  , _ctxClassImports :: Set qcn
  , _ctxRuleImports :: Set (PC.InfoLess PC.ModuleName)
  , _ctxTyExports :: Set (PC.InfoLess PC.TyName)
  , _ctxConfig :: Config qtn qcn
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'MkContext

runPrint ::
  forall qtn qcn.
  Context qtn qcn ->
  PrintM qtn qcn (Doc ()) ->
  Either P.CompilerError (Doc ())
runPrint ctx modPrinter =
  let p = runReaderT modPrinter ctx
   in case runExcept p of
        Left err ->
          Left $
            defMessage
              & P.internalErrors
                .~ [ defMessage
                      & P.msg .~ (Text.pack . show $ err) -- TODO(bladyjoker): Introduce SourceInfo in InternalError.
                   ]
        Right doc -> Right doc
