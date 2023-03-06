module LambdaBuffers.Codegen.Print (
  runPrint,
  Context (..),
  MonadPrint,
  ctxLbTyImports,
  ctxOpaqueTyImports,
  ctxTyExports,
  ctxConfig,
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

type MonadPrint o c m = (MonadError Error m, MonadReader (Context o c) m)

type PrintM o c = ReaderT (Context o c) (Except Error)

type Error = (PC.SourceInfo, Text)

data Context o c = MkContext
  { _ctxModule :: PC.Module
  , _ctxLbTyImports :: Set PC.QTyName
  , _ctxOpaqueTyImports :: Set o
  , _ctxTyExports :: Set (PC.InfoLess PC.TyName)
  , _ctxConfig :: Config o c
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'MkContext

runPrint ::
  forall o c.
  Context o c ->
  PrintM o c (Doc ()) ->
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
