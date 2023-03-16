module LambdaBuffers.Frontend.ToProto (toCompilerInput) where

import Control.Lens ((&), (.~))
import Control.Monad (void)
import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Traversable (for)
import LambdaBuffers.Frontend (FrontendResult (FrontendResult), Scope)
import LambdaBuffers.Frontend.Syntax (
  ConstrName (ConstrName),
  Constructor (Constructor),
  Field (Field),
  FieldName (FieldName),
  Module (Module),
  ModuleName (ModuleName),
  ModuleNamePart (ModuleNamePart),
  Product (Product),
  Record (Record),
  SourceInfo (SourceInfo),
  SourcePos (SourcePos),
  Sum (Sum),
  Ty (TyApp, TyRef', TyVar),
  TyArg (TyArg),
  TyBody (Opaque, ProductBody, RecordBody, SumBody),
  TyDef (TyDef),
  TyName (TyName),
  TyRef (TyRef),
  VarName (VarName),
 )
import Proto.Compiler (Kind'KindRef (Kind'KIND_REF_TYPE))
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

type ToProto a = (ReaderT (ModuleName (), Scope) (Except ToProtoError)) a

newtype ToProtoError = MissingTyRef (TyRef SourceInfo)
  deriving stock (Show, Eq)

toCompilerInput :: FrontendResult -> Either ToProtoError P.CompilerInput
toCompilerInput (FrontendResult modsWithScope) = do
  mods <-
    for
      (Map.toList modsWithScope)
      ( \(modName, (m, scope)) -> do
          let errM = runReaderT (toModule m) (modName, scope)
          runExcept errM
      )
  return $ defMessage & P.modules .~ mods

toModule :: Module SourceInfo -> ToProto P.Module
toModule (Module mn _ tyds info) = do
  tyds' <- for tyds toTypeDef
  return $
    defMessage
      & P.moduleName .~ toModuleName mn
      & P.typeDefs .~ tyds'
      & P.sourceInfo .~ toSourceInfo info

toTypeDef :: TyDef SourceInfo -> ToProto P.TyDef
toTypeDef (TyDef tn args body info) = do
  let tydef =
        defMessage
          & P.tyName .~ toTyName tn
          & P.sourceInfo
            .~ toSourceInfo
              info
  abs' <- toTyAbs args body
  return $ tydef & P.tyAbs .~ abs'

toTyBody :: TyBody SourceInfo -> ToProto P.TyBody
toTyBody (SumBody s) = do
  s' <- toSum s
  return $ defMessage & P.sum .~ s'
toTyBody (ProductBody p) = do
  p' <- toProduct p
  return $ defMessage & P.product .~ p'
toTyBody (RecordBody r) = do
  r' <- toRecord r
  return $ defMessage & P.record .~ r'
toTyBody Opaque = return $ defMessage & P.opaque .~ defMessage

-- TODO(bladyjoker): Add SourceInfo?
toConstructor :: Constructor SourceInfo -> ToProto P.Sum'Constructor
toConstructor (Constructor cname prod _info) = do
  prod' <- toProduct prod
  return $
    defMessage
      & P.constrName .~ toConstName cname
      & P.product .~ prod'

toSum :: Sum SourceInfo -> ToProto P.Sum
toSum (Sum cs info) = do
  cs' <- for cs toConstructor
  return $
    defMessage
      & P.constructors .~ cs'
      & P.sourceInfo .~ toSourceInfo info

toProduct :: Product SourceInfo -> ToProto P.Product
toProduct (Product tys info) = do
  tys' <- for tys toTy
  return $
    defMessage
      & P.fields .~ tys'
      & P.sourceInfo .~ toSourceInfo info

toRecord :: Record SourceInfo -> ToProto P.Record
toRecord (Record fields info) = do
  fields' <- for fields toField
  return $
    defMessage
      & P.fields .~ fields'
      & P.sourceInfo .~ toSourceInfo info

-- TODO(bladyjoker): Remove info if unnecessary.
toField :: Field SourceInfo -> ToProto P.Record'Field
toField (Field fn ty _info) = do
  ty' <- toTy ty
  return $
    defMessage
      & P.fieldName .~ toFieldName fn
      & P.fieldTy .~ ty'

toTy :: Ty SourceInfo -> ToProto P.Ty
toTy (TyVar vn) =
  return $
    defMessage
      & P.tyVar . P.varName .~ toVarName vn
toTy (TyApp ty tys info) = do
  ty' <- toTy ty
  tys' <- for tys toTy
  return $
    defMessage
      & P.tyApp . P.tyFunc .~ ty'
      & P.tyApp . P.tyArgs .~ tys'
      & P.tyApp . P.sourceInfo .~ toSourceInfo info
toTy (TyRef' tref@(TyRef _ tn info) _) = do
  (currentModName, scope) <- ask
  case Map.lookup (void tref) scope of
    Nothing -> throwError $ MissingTyRef tref
    Just modName ->
      if currentModName == void modName
        then
          return $
            defMessage
              & P.tyRef . P.localTyRef . P.tyName .~ toTyName tn
              & P.tyRef . P.localTyRef . P.sourceInfo .~ toSourceInfo info
        else
          return $
            defMessage
              & P.tyRef . P.foreignTyRef . P.moduleName .~ toModuleName modName
              & P.tyRef . P.foreignTyRef . P.tyName .~ toTyName tn
              & P.tyRef . P.foreignTyRef . P.sourceInfo .~ toSourceInfo info

toVarName :: VarName SourceInfo -> P.VarName
toVarName (VarName n info) =
  defMessage
    & P.name .~ n
    & P.sourceInfo .~ toSourceInfo info

toConstName :: ConstrName SourceInfo -> P.ConstrName
toConstName (ConstrName cn info) =
  defMessage
    & P.name .~ cn
    & P.sourceInfo .~ toSourceInfo info

toFieldName :: FieldName SourceInfo -> P.FieldName
toFieldName (FieldName fn info) =
  defMessage
    & P.name .~ fn
    & P.sourceInfo .~ toSourceInfo info

-- TODO(bladyjoker): TyAbs needs SourceInfo? Remove it if not.
toTyAbs :: [TyArg SourceInfo] -> TyBody SourceInfo -> ToProto P.TyAbs
toTyAbs args body = do
  body' <- toTyBody body
  return $
    defMessage
      & P.tyArgs .~ (toTyArg <$> args)
      & P.tyBody .~ body'

toTyArg :: TyArg SourceInfo -> P.TyArg
toTyArg (TyArg an info) =
  defMessage
    & P.argName . P.name .~ an
    & P.argKind . P.kindRef .~ Kind'KIND_REF_TYPE
    & P.sourceInfo .~ toSourceInfo info

toTyName :: TyName SourceInfo -> P.TyName
toTyName (TyName n info) =
  defMessage
    & P.name .~ n
    & P.sourceInfo .~ toSourceInfo info

toSourceInfo :: SourceInfo -> P.SourceInfo
toSourceInfo (SourceInfo f fr to) =
  defMessage
    & P.file .~ f
    & P.posFrom .~ toSourcePos fr
    & P.posTo .~ toSourcePos to

toSourcePos :: SourcePos -> P.SourcePosition
toSourcePos (SourcePos r c) =
  defMessage
    & P.row .~ fromIntegral r
    & P.column .~ fromIntegral c

toModuleName :: ModuleName SourceInfo -> P.ModuleName
toModuleName (ModuleName ps info) =
  defMessage
    & P.parts .~ (toModuleNamePart <$> ps)
    & P.sourceInfo .~ toSourceInfo info

toModuleNamePart :: ModuleNamePart SourceInfo -> P.ModuleNamePart
toModuleNamePart (ModuleNamePart p info) =
  defMessage
    & P.name .~ p
    & P.sourceInfo .~ toSourceInfo info
