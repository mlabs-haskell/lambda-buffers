module LambdaBuffers.Frontend.ToProto (toCompilerInput) where

import Control.Lens ((&), (.~))
import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import LambdaBuffers.Frontend (FrontendResult (FrontendResult))
import LambdaBuffers.Frontend.Syntax (
  ClassConstraint (ClassConstraint),
  ClassDef (ClassDef),
  ClassName (ClassName),
  ClassRef (ClassRef),
  ConstrName (ConstrName),
  Constraint (Constraint),
  Constructor (Constructor),
  Derive (Derive),
  Field (Field),
  FieldName (FieldName),
  InstanceClause (InstanceClause),
  Module (Module),
  ModuleName (ModuleName),
  ModuleNamePart (ModuleNamePart),
  Product (Product),
  Record (Record),
  SourceInfo (SourceInfo),
  SourcePos (SourcePos),
  Statement (StClassDef, StDerive, StInstanceClause, StTyDef),
  Sum (Sum),
  Ty (TyApp, TyRef', TyVar),
  TyArg (TyArg),
  TyBody (Opaque, ProductBody, RecordBody, SumBody),
  TyDef (TyDef),
  TyName (TyName),
  TyRef (TyRef),
  VarName (VarName),
  defSourceInfo,
 )
import LambdaBuffers.Frontend.Utils (Scope, strip)
import Proto.Compiler (Kind'KindRef (Kind'KIND_REF_TYPE))
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

type ToProto a = (ReaderT (ModuleName (), Scope) (Except ToProtoError)) a

data ToProtoError = MissingTyRef (TyRef SourceInfo) | MissingClassRef (ClassRef SourceInfo)
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
toModule (Module mn _ stmts info) = do
  tyds <- for [td | StTyDef td <- stmts] toTypeDef
  clds <- for [cd | StClassDef cd <- stmts] toClassDef
  ics <- for [ic | StInstanceClause ic <- stmts] toInstanceClause
  ds <- for [d | StDerive d <- stmts] toDerive
  imps <- toImports
  return $
    defMessage
      & P.moduleName .~ toModuleName mn
      & P.typeDefs .~ tyds
      & P.classDefs .~ clds
      & P.instances .~ ics
      & P.derives .~ ds
      & P.imports .~ toList imps
      & P.sourceInfo .~ toSourceInfo info

-- FIXME(bladyjoker): Implement ImportCycle check in Compiler (otherwise Codegen does a cycled import).
toImports :: ToProto (Set P.ModuleName)
toImports = do
  (currMn, (tyScope, classScope)) <- ask
  let foreignModules = filter (/= currMn) $ toList tyScope <> toList classScope
  return $ Set.fromList . fmap (\mn -> toModuleName (defSourceInfo <$ mn)) $ foreignModules

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

toClassDef :: ClassDef SourceInfo -> ToProto P.ClassDef
toClassDef (ClassDef cn args sups info) = do
  sups' <- toClassConstraint `traverse` sups
  return $
    defMessage
      & P.className .~ toClassName cn
      & P.classArgs .~ (toTyArg <$> args)
      & P.supers .~ sups'
      & P.sourceInfo
        .~ toSourceInfo
          info

toInstanceClause :: InstanceClause SourceInfo -> ToProto P.InstanceClause
toInstanceClause (InstanceClause h body info) = do
  h' <- toConstraint h
  body' <- for body toConstraint
  return $
    defMessage
      & P.head .~ h'
      & P.constraints .~ body'
      & P.sourceInfo
        .~ toSourceInfo
          info

toDerive :: Derive SourceInfo -> ToProto P.Derive
toDerive (Derive cstr) = do
  cstr' <- toConstraint cstr
  return $
    defMessage
      & P.constraint .~ cstr'

toConstraint :: Constraint SourceInfo -> ToProto P.Constraint
toConstraint (Constraint cr args info) = do
  cr' <- toClassRef cr
  args' <- for args toTy
  return $
    defMessage
      & P.classRef .~ cr'
      & P.args .~ args'
      & P.sourceInfo
        .~ toSourceInfo
          info

-- TODO(bladyjoker): ClassConstraint should have TyVars not TyArgs.
toClassConstraint :: ClassConstraint SourceInfo -> ToProto P.ClassConstraint
toClassConstraint (ClassConstraint cr args) = do
  cr' <- toClassRef cr
  return $
    defMessage
      & P.classRef .~ cr'
      & P.args .~ (toTyVar <$> args)

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
toTy (TyRef' tr _) = toTyRef tr

toTyRef :: TyRef SourceInfo -> ToProto P.Ty
toTyRef tref@(TyRef mayAlias tn info) = do
  (currentModName, (tyScope, _)) <- ask
  case Map.lookup (strip <$> mayAlias, strip tn) tyScope of
    Nothing -> throwError $ MissingTyRef tref
    Just modName ->
      if strip currentModName == modName
        then
          return $
            defMessage
              & P.tyRef . P.localTyRef . P.tyName .~ toTyName tn
              & P.tyRef . P.localTyRef . P.sourceInfo .~ toSourceInfo info
        else
          return $
            defMessage
              & P.tyRef . P.foreignTyRef . P.moduleName .~ toModuleName (defSourceInfo <$ modName) -- NOTE(bladyjoker): ModuleName in ForeignTyRef don't have a Frontend Syntax representation.
              & P.tyRef . P.foreignTyRef . P.tyName .~ toTyName tn
              & P.tyRef . P.foreignTyRef . P.sourceInfo .~ toSourceInfo info

toClassRef :: ClassRef SourceInfo -> ToProto P.TyClassRef
toClassRef tref@(ClassRef mayAlias cn info) = do
  (currentModName, (_, classScope)) <- ask
  case Map.lookup (strip <$> mayAlias, strip cn) classScope of
    Nothing -> throwError $ MissingClassRef tref
    Just modName ->
      if strip currentModName == modName
        then
          return $
            defMessage
              & P.localClassRef . P.className .~ toClassName cn
              & P.localClassRef . P.sourceInfo .~ toSourceInfo info
        else
          return $
            defMessage
              & P.foreignClassRef . P.moduleName .~ toModuleName (defSourceInfo <$ modName) -- NOTE(bladyjoker): ModuleName in ForeignClassRef don't have a Frontend Syntax representation.
              & P.foreignClassRef . P.className .~ toClassName cn
              & P.foreignClassRef . P.sourceInfo .~ toSourceInfo info

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

toTyVar :: TyArg SourceInfo -> P.TyVar
toTyVar (TyArg an info) =
  defMessage
    & P.varName . P.name .~ an
    & P.varName . P.sourceInfo .~ toSourceInfo info

toTyName :: TyName SourceInfo -> P.TyName
toTyName (TyName n info) =
  defMessage
    & P.name .~ n
    & P.sourceInfo .~ toSourceInfo info

toClassName :: ClassName SourceInfo -> P.ClassName
toClassName (ClassName n info) =
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
