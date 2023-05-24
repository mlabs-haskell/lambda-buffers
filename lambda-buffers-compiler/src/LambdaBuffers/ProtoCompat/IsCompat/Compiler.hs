{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.ProtoCompat.IsCompat.Compiler (compilerInputFromProto) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader (local))
import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.ProtoLens (Message (messageName), defMessage)
import Data.Proxy (Proxy (Proxy))
import LambdaBuffers.ProtoCompat.InfoLess (mkInfoLess)
import LambdaBuffers.ProtoCompat.IsCompat.FromProto (FromProtoContext (CtxCompilerInput), FromProtoErr (FPProtoParseError), IsCompat (fromProto, toProto), runFromProto, throwInternalError, throwOneOfError)
import LambdaBuffers.ProtoCompat.IsCompat.Lang ()
import LambdaBuffers.ProtoCompat.IsCompat.Utils (parseAndIndex)
import LambdaBuffers.ProtoCompat.Types (CompilerInput)
import LambdaBuffers.ProtoCompat.Types qualified as Compat
import Proto.Compiler qualified as Compiler
import Proto.Compiler_Fields qualified as Compiler

compilerInputFromProto :: Compiler.Input -> Either Compiler.Error CompilerInput
compilerInputFromProto = runFromProto CtxCompilerInput

instance IsCompat Compiler.Input Compat.CompilerInput where
  fromProto ci = do
    local (const CtxCompilerInput) $ do
      (mods, mulModules) <- parseAndIndex (\m -> mkInfoLess $ m ^. #moduleName) (ci ^. Compiler.modules)
      let mulModulesErrs =
            [ FPProtoParseError $
              defMessage & Compiler.multipleModuleError . Compiler.modules .~ ms
            | (_mn, ms) <- Map.toList mulModules
            ]
      if null mulModulesErrs
        then pure $ Compat.CompilerInput mods
        else throwError mulModulesErrs

  toProto (Compat.CompilerInput ms) =
    defMessage
      & Compiler.modules .~ (toProto <$> toList ms)

{-
  Outputs
-}

instance IsCompat Compiler.KindCheckError Compat.KindCheckError where
  fromProto kce =
    case kce ^. Compiler.maybe'kindCheckError of
      Just x -> case x of
        Compiler.KindCheckError'UnboundTyVarError' err ->
          Compat.UnboundTyVarError
            <$> fromProto (err ^. Compiler.tyDef)
            <*> fromProto (err ^. Compiler.tyVar)
            <*> fromProto (err ^. Compiler.moduleName)
        Compiler.KindCheckError'UnboundTyRefError' err ->
          Compat.UnboundTyRefError
            <$> fromProto (err ^. Compiler.tyDef)
            <*> fromProto (err ^. Compiler.tyRef)
            <*> fromProto (err ^. Compiler.moduleName)
        Compiler.KindCheckError'UnificationError' err ->
          Compat.IncorrectApplicationError
            <$> fromProto (err ^. Compiler.tyDef)
            <*> fromProto (err ^. Compiler.tyKindLhs)
            <*> fromProto (err ^. Compiler.tyKindRhs)
            <*> fromProto (err ^. Compiler.moduleName)
        Compiler.KindCheckError'CyclicKindError' err ->
          Compat.RecursiveKindError
            <$> fromProto (err ^. Compiler.tyDef)
            <*> fromProto (err ^. Compiler.moduleName)
        Compiler.KindCheckError'InconsistentTypeError' err ->
          Compat.InconsistentTypeError
            <$> fromProto (err ^. Compiler.tyDef)
            <*> fromProto (err ^. Compiler.actualKind)
            <*> fromProto (err ^. Compiler.expectedKind)
            <*> fromProto (err ^. Compiler.moduleName)
      Nothing -> throwOneOfError (messageName (Proxy @Compiler.KindCheckError)) "kind_check_error"

  toProto = \case
    Compat.UnboundTyVarError tydef tyvar modname ->
      defMessage
        & (Compiler.unboundTyVarError . Compiler.tyDef) .~ toProto tydef
        & (Compiler.unboundTyVarError . Compiler.tyVar) .~ toProto tyvar
        & (Compiler.unboundTyVarError . Compiler.moduleName) .~ toProto modname
    Compat.UnboundTyRefError tydef tyref modname ->
      defMessage
        & (Compiler.unboundTyRefError . Compiler.tyDef) .~ toProto tydef
        & (Compiler.unboundTyRefError . Compiler.tyRef) .~ toProto tyref
        & (Compiler.unboundTyRefError . Compiler.moduleName) .~ toProto modname
    Compat.IncorrectApplicationError tydef k1 k2 modname ->
      defMessage
        & (Compiler.unificationError . Compiler.tyDef) .~ toProto tydef
        & (Compiler.unificationError . Compiler.tyKindLhs) .~ toProto k1
        & (Compiler.unificationError . Compiler.tyKindRhs) .~ toProto k2
        & (Compiler.unificationError . Compiler.moduleName) .~ toProto modname
    Compat.RecursiveKindError tydef modname ->
      defMessage
        & (Compiler.cyclicKindError . Compiler.tyDef) .~ toProto tydef
        & (Compiler.cyclicKindError . Compiler.moduleName) .~ toProto modname
    Compat.InconsistentTypeError tydef ki kd modname ->
      defMessage
        & (Compiler.inconsistentTypeError . Compiler.tyDef) .~ toProto tydef
        & (Compiler.inconsistentTypeError . Compiler.actualKind) .~ toProto ki
        & (Compiler.inconsistentTypeError . Compiler.expectedKind) .~ toProto kd
        & (Compiler.inconsistentTypeError . Compiler.moduleName) .~ toProto modname

instance IsCompat Compiler.Error Compat.CompilerError where
  fromProto _ = throwInternalError "fromProto CompilerError not implemented"

  toProto = \case
    Compat.CompKindCheckError err -> defMessage & Compiler.kindCheckErrors .~ [toProto err]
    Compat.InternalError err -> defMessage & Compiler.internalErrors .~ [defMessage & Compiler.msg .~ err]

instance IsCompat Compiler.Result Compat.CompilerResult where
  fromProto _ = throwInternalError "fromProto CompilerError not implemented"
  toProto Compat.CompilerResult = defMessage

instance IsCompat Compiler.Output Compat.CompilerOutput where
  fromProto _ = throwInternalError "fromProto CompilerError not implemented"

  toProto = \case
    Right res -> defMessage & Compiler.result .~ toProto res
    Left err -> defMessage & Compiler.error .~ toProto err
