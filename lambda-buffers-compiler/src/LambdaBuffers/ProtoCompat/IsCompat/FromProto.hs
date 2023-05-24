module LambdaBuffers.ProtoCompat.IsCompat.FromProto (
  runFromProto,
  FromProto,
  FromProtoErr (..),
  FromProtoContext (..),
  IsCompat (..),
  throwNamingError,
  throwOneOfError,
  throwInternalError,
) where

import Control.Lens ((&), (.~))
import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Kind (Type)
import Data.ProtoLens (defMessage)
import Data.Text (Text)
import GHC.Generics (Generic)
import Proto.Compiler qualified as Compiler
import Proto.Compiler_Fields qualified as Compiler
import Proto.Lang qualified as Lang

class IsCompat (proto :: Type) (good :: Type) where
  fromProto :: proto -> FromProto good

  toProto :: good -> proto

data FromProtoErr
  = FPNamingError Compiler.NamingError
  | FPInternalError Compiler.InternalError
  | FPProtoParseError Compiler.ProtoParseError
  deriving stock (Show, Eq, Ord, Generic)

data FromProtoContext
  = CtxCompilerInput
  | CtxModule Lang.ModuleName
  | CtxTyDef Lang.ModuleName Lang.TyDef
  | CtxClassDef Lang.ModuleName Lang.ClassDef
  deriving stock (Show, Eq, Ord, Generic)

type FromProto a = ReaderT FromProtoContext (Except [FromProtoErr]) a

-- | Parse a Proto API message into the internal representation or report errors (in Proto format).
runFromProto :: IsCompat proto good => FromProtoContext -> proto -> Either Compiler.Error good
runFromProto ctx proto = do
  let exM = runReaderT (fromProto proto) ctx
      errsOrRes = runExcept exM
  case errsOrRes of
    Left errs ->
      let nerrs = [err | FPNamingError err <- errs]
          pperrs = [err | FPProtoParseError err <- errs]
          ierrs = [err | FPInternalError err <- errs]
       in Left $
            defMessage
              & Compiler.namingErrors .~ nerrs
              & Compiler.protoParseErrors .~ pperrs
              & Compiler.internalErrors .~ ierrs
    Right good -> return good

throwNamingError :: Either Compiler.NamingError b -> FromProto b
throwNamingError = either (\err -> throwError [FPNamingError err]) return

throwOneOfError :: Text -> Text -> FromProto b
throwOneOfError protoMsgName protoFieldName =
  throwError
    [ FPProtoParseError $
        defMessage
          & Compiler.oneOfNotSetError . Compiler.messageName .~ protoMsgName
          & Compiler.oneOfNotSetError . Compiler.fieldName .~ protoFieldName
    ]

throwInternalError :: Text -> FromProto b
throwInternalError msg = throwError [FPInternalError $ defMessage & Compiler.msg .~ msg]
