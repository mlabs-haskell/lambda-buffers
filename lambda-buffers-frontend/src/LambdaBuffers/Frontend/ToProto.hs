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
  Module (Module),
  ModuleName (ModuleName),
  ModuleNamePart (ModuleNamePart),
  Product (Product),
  SourceInfo (SourceInfo),
  SourcePos (SourcePos),
  Ty (TyApp, TyRef', TyVar),
  TyArg (TyArg),
  TyBody (Opaque, Sum),
  TyDef (TyDef),
  TyName (TyName),
  TyRef (TyRef),
  VarName (VarName),
 )
import Proto.Compiler (Kind'KindRef (Kind'KIND_REF_TYPE))
import Proto.Compiler qualified as P
import Proto.Compiler_Fields as PF (argKind, argName, column, constrName, constructors, fields, file, foreignTyRef, kindRef, localTyRef, moduleName, modules, name, ntuple, opaque, parts, posFrom, posTo, product, row, sourceInfo, sum, tyAbs, tyApp, tyArgs, tyBody, tyFunc, tyName, tyRef, tyVar, typeDefs, varName)

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
  return $ defMessage & modules .~ mods

toModule :: Module SourceInfo -> ToProto P.Module
toModule (Module mn _ tyds info) = do
  tyds' <- for tyds toTypeDef
  return $
    defMessage
      & moduleName .~ toModuleName mn
      & typeDefs .~ tyds'
      & sourceInfo .~ toSourceInfo info

toTypeDef :: TyDef SourceInfo -> ToProto P.TyDef
toTypeDef (TyDef tn args body info) =
  let tydef =
        defMessage
          & tyName .~ toTyName tn
          & sourceInfo
            .~ toSourceInfo
              info
   in case args of
        [] -> do
          body' <- toTyBody body
          return $ tydef & tyBody .~ body'
        (a : as) -> do
          abs' <- toTyAbs a as body
          return $ tydef & tyAbs .~ abs'

toTyBody :: TyBody SourceInfo -> ToProto P.TyBody
toTyBody (Sum cs info) = do
  cs' <- for cs toConstructor
  return $
    defMessage
      & PF.sum . constructors .~ cs'
      & PF.sum . sourceInfo .~ toSourceInfo info
toTyBody Opaque = return $ defMessage & opaque .~ defMessage

-- TODO(bladyjoker): Add SourceInfo?
toConstructor :: Constructor SourceInfo -> ToProto P.Sum'Constructor
toConstructor (Constructor cname prod _info) = do
  prod' <- toProduct prod
  return $
    defMessage
      & constrName .~ toConstName cname
      & PF.product .~ prod'

toProduct :: Product SourceInfo -> ToProto P.Product
toProduct (Product tys info) = do
  tys' <- for tys toTy
  return $
    defMessage
      & ntuple . fields .~ tys'
      & ntuple . sourceInfo .~ toSourceInfo info

toTy :: Ty SourceInfo -> ToProto P.Ty
toTy (TyVar vn info) =
  return $
    defMessage
      & tyVar . varName .~ toVarName vn
      & tyVar . sourceInfo .~ toSourceInfo info
toTy (TyApp ty tys info) = do
  ty' <- toTy ty
  tys' <- for tys toTy
  return $
    defMessage
      & tyApp . tyFunc .~ ty'
      & tyApp . tyArgs .~ tys'
      & tyApp . sourceInfo .~ toSourceInfo info
toTy (TyRef' tref@(TyRef _ tn info) _) = do
  (currentModName, scope) <- ask
  case Map.lookup (void tref) scope of
    Nothing -> throwError $ MissingTyRef tref
    Just modName ->
      if currentModName == void modName
        then
          return $
            defMessage
              & tyRef . localTyRef . tyName .~ toTyName tn
              & tyRef . localTyRef . sourceInfo .~ toSourceInfo info
        else
          return $
            defMessage
              & tyRef . foreignTyRef . moduleName .~ toModuleName modName
              & tyRef . foreignTyRef . tyName .~ toTyName tn
              & tyRef . foreignTyRef . sourceInfo .~ toSourceInfo info

toVarName :: VarName SourceInfo -> P.VarName
toVarName (VarName n info) =
  defMessage
    & name .~ n
    & sourceInfo .~ toSourceInfo info

toConstName :: ConstrName SourceInfo -> P.ConstrName
toConstName (ConstrName cn info) =
  defMessage
    & name .~ cn
    & sourceInfo .~ toSourceInfo info

-- TODO(bladyjoker): TyAbs needs SourceInfo? Remove it if not.
toTyAbs :: TyArg SourceInfo -> [TyArg SourceInfo] -> TyBody SourceInfo -> ToProto P.TyAbs
toTyAbs arg args body = do
  body' <- toTyBody body
  return $
    defMessage
      & tyArgs .~ (toTyArg <$> arg : args)
      & tyBody .~ body'

toTyArg :: TyArg SourceInfo -> P.TyArg
toTyArg (TyArg an info) =
  defMessage
    & argName . name .~ an
    & argKind . kindRef .~ Kind'KIND_REF_TYPE
    & sourceInfo .~ toSourceInfo info

toTyName :: TyName SourceInfo -> P.TyName
toTyName (TyName n info) =
  defMessage
    & name .~ n
    & sourceInfo .~ toSourceInfo info

toSourceInfo :: SourceInfo -> P.SourceInfo
toSourceInfo (SourceInfo f fr to) =
  defMessage
    & file .~ f
    & posFrom .~ toSourcePos fr
    & posTo .~ toSourcePos to

toSourcePos :: SourcePos -> P.SourcePosition
toSourcePos (SourcePos r c) =
  defMessage
    & row .~ fromIntegral r
    & column .~ fromIntegral c

toModuleName :: ModuleName SourceInfo -> P.ModuleName
toModuleName (ModuleName ps info) =
  defMessage
    & parts .~ (toModuleNamePart <$> ps)
    & sourceInfo .~ toSourceInfo info

toModuleNamePart :: ModuleNamePart SourceInfo -> P.ModuleNamePart
toModuleNamePart (ModuleNamePart p info) =
  defMessage
    & name .~ p
    & sourceInfo .~ toSourceInfo info
