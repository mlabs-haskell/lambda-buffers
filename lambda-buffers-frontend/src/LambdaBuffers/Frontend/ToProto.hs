module LambdaBuffers.Frontend.ToProto (toModules) where

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
import Proto.Lang qualified as Lang
import Proto.Lang_Fields qualified as Lang

type ToProto a = (ReaderT (ModuleName (), Scope) (Except ToProtoError)) a

data ToProtoError = MissingTyRef (TyRef SourceInfo) | MissingClassRef (ClassRef SourceInfo)
  deriving stock (Show, Eq)

toModules :: FrontendResult -> Either ToProtoError [Lang.Module]
toModules (FrontendResult modsWithScope _requestedModuleNames) =
  for
    (Map.toList modsWithScope)
    ( \(modName, (m, scope)) -> do
        let errM = runReaderT (toModule m) (modName, scope)
        runExcept errM
    )

toModule :: Module SourceInfo -> ToProto Lang.Module
toModule (Module mn _ stmts info) = do
  tyds <- for [td | StTyDef td <- stmts] toTypeDef
  clds <- for [cd | StClassDef cd <- stmts] toClassDef
  ics <- for [ic | StInstanceClause ic <- stmts] toInstanceClause
  ds <- for [d | StDerive d <- stmts] toDerive
  imps <- toImports
  return $
    defMessage
      & Lang.moduleName .~ toModuleName mn
      & Lang.typeDefs .~ tyds
      & Lang.classDefs .~ clds
      & Lang.instances .~ ics
      & Lang.derives .~ ds
      & Lang.imports .~ toList imps
      & Lang.sourceInfo .~ toSourceInfo info

-- FIXME(bladyjoker): Implement ImportCycle check in Compiler (otherwise Codegen does a cycled import).
toImports :: ToProto (Set Lang.ModuleName)
toImports = do
  (currMn, (tyScope, classScope)) <- ask
  let foreignModules = filter (/= currMn) $ toList tyScope <> toList classScope
  return $ Set.fromList . fmap (\mn -> toModuleName (defSourceInfo <$ mn)) $ foreignModules

toTypeDef :: TyDef SourceInfo -> ToProto Lang.TyDef
toTypeDef (TyDef tn args body info) = do
  let tydef =
        defMessage
          & Lang.tyName .~ toTyName tn
          & Lang.sourceInfo
            .~ toSourceInfo
              info
  abs' <- toTyAbs args body
  return $ tydef & Lang.tyAbs .~ abs'

toClassDef :: ClassDef SourceInfo -> ToProto Lang.ClassDef
toClassDef (ClassDef cn args sups info) = do
  sups' <- toClassConstraint `traverse` sups
  return $
    defMessage
      & Lang.className .~ toClassName cn
      & Lang.classArgs .~ (toTyArg <$> args)
      & Lang.supers .~ sups'
      & Lang.sourceInfo
        .~ toSourceInfo
          info

toInstanceClause :: InstanceClause SourceInfo -> ToProto Lang.InstanceClause
toInstanceClause (InstanceClause h body info) = do
  h' <- toConstraint h
  body' <- for body toConstraint
  return $
    defMessage
      & Lang.head .~ h'
      & Lang.constraints .~ body'
      & Lang.sourceInfo
        .~ toSourceInfo
          info

toDerive :: Derive SourceInfo -> ToProto Lang.Derive
toDerive (Derive cstr) = do
  cstr' <- toConstraint cstr
  return $
    defMessage
      & Lang.constraint .~ cstr'

toConstraint :: Constraint SourceInfo -> ToProto Lang.Constraint
toConstraint (Constraint cr args info) = do
  cr' <- toClassRef cr
  args' <- for args toTy
  return $
    defMessage
      & Lang.classRef .~ cr'
      & Lang.args .~ args'
      & Lang.sourceInfo
        .~ toSourceInfo
          info

-- TODO(bladyjoker): ClassConstraint should have TyVars not TyArgs.
toClassConstraint :: ClassConstraint SourceInfo -> ToProto Lang.ClassConstraint
toClassConstraint (ClassConstraint cr args) = do
  cr' <- toClassRef cr
  return $
    defMessage
      & Lang.classRef .~ cr'
      & Lang.args .~ (toTyVar <$> args)

toTyBody :: TyBody SourceInfo -> ToProto Lang.TyBody
toTyBody (SumBody s) = do
  s' <- toSum s
  return $ defMessage & Lang.sum .~ s'
toTyBody (ProductBody p) = do
  p' <- toProduct p
  return $ defMessage & Lang.product .~ p'
toTyBody (RecordBody r) = do
  r' <- toRecord r
  return $ defMessage & Lang.record .~ r'
toTyBody Opaque = return $ defMessage & Lang.opaque .~ defMessage

-- TODO(bladyjoker): Add SourceInfo?
toConstructor :: Constructor SourceInfo -> ToProto Lang.Sum'Constructor
toConstructor (Constructor cname prod _info) = do
  prod' <- toProduct prod
  return $
    defMessage
      & Lang.constrName .~ toConstName cname
      & Lang.product .~ prod'

toSum :: Sum SourceInfo -> ToProto Lang.Sum
toSum (Sum cs info) = do
  cs' <- for cs toConstructor
  return $
    defMessage
      & Lang.constructors .~ cs'
      & Lang.sourceInfo .~ toSourceInfo info

toProduct :: Product SourceInfo -> ToProto Lang.Product
toProduct (Product tys info) = do
  tys' <- for tys toTy
  return $
    defMessage
      & Lang.fields .~ tys'
      & Lang.sourceInfo .~ toSourceInfo info

toRecord :: Record SourceInfo -> ToProto Lang.Record
toRecord (Record fields info) = do
  fields' <- for fields toField
  return $
    defMessage
      & Lang.fields .~ fields'
      & Lang.sourceInfo .~ toSourceInfo info

-- TODO(bladyjoker): Remove info if unnecessary.
toField :: Field SourceInfo -> ToProto Lang.Record'Field
toField (Field fn ty _info) = do
  ty' <- toTy ty
  return $
    defMessage
      & Lang.fieldName .~ toFieldName fn
      & Lang.fieldTy .~ ty'

toTy :: Ty SourceInfo -> ToProto Lang.Ty
toTy (TyVar vn) =
  return $
    defMessage
      & Lang.tyVar . Lang.varName .~ toVarName vn
toTy (TyApp ty tys info) = do
  ty' <- toTy ty
  tys' <- for tys toTy
  return $
    defMessage
      & Lang.tyApp . Lang.tyFunc .~ ty'
      & Lang.tyApp . Lang.tyArgs .~ tys'
      & Lang.tyApp . Lang.sourceInfo .~ toSourceInfo info
toTy (TyRef' tr _) = toTyRef tr

toTyRef :: TyRef SourceInfo -> ToProto Lang.Ty
toTyRef tref@(TyRef mayAlias tn info) = do
  (currentModName, (tyScope, _)) <- ask
  case Map.lookup (strip <$> mayAlias, strip tn) tyScope of
    Nothing -> throwError $ MissingTyRef tref
    Just modName ->
      if strip currentModName == modName
        then
          return $
            defMessage
              & Lang.tyRef . Lang.localTyRef . Lang.tyName .~ toTyName tn
              & Lang.tyRef . Lang.localTyRef . Lang.sourceInfo .~ toSourceInfo info
        else
          return $
            defMessage
              & Lang.tyRef . Lang.foreignTyRef . Lang.moduleName .~ toModuleName (defSourceInfo <$ modName) -- NOTE(bladyjoker): ModuleName in ForeignTyRef don't have a Frontend Syntax representation.
              & Lang.tyRef . Lang.foreignTyRef . Lang.tyName .~ toTyName tn
              & Lang.tyRef . Lang.foreignTyRef . Lang.sourceInfo .~ toSourceInfo info

toClassRef :: ClassRef SourceInfo -> ToProto Lang.TyClassRef
toClassRef tref@(ClassRef mayAlias cn info) = do
  (currentModName, (_, classScope)) <- ask
  case Map.lookup (strip <$> mayAlias, strip cn) classScope of
    Nothing -> throwError $ MissingClassRef tref
    Just modName ->
      if strip currentModName == modName
        then
          return $
            defMessage
              & Lang.localClassRef . Lang.className .~ toClassName cn
              & Lang.localClassRef . Lang.sourceInfo .~ toSourceInfo info
        else
          return $
            defMessage
              & Lang.foreignClassRef . Lang.moduleName .~ toModuleName (defSourceInfo <$ modName) -- NOTE(bladyjoker): ModuleName in ForeignClassRef don't have a Frontend Syntax representation.
              & Lang.foreignClassRef . Lang.className .~ toClassName cn
              & Lang.foreignClassRef . Lang.sourceInfo .~ toSourceInfo info

toVarName :: VarName SourceInfo -> Lang.VarName
toVarName (VarName n info) =
  defMessage
    & Lang.name .~ n
    & Lang.sourceInfo .~ toSourceInfo info

toConstName :: ConstrName SourceInfo -> Lang.ConstrName
toConstName (ConstrName cn info) =
  defMessage
    & Lang.name .~ cn
    & Lang.sourceInfo .~ toSourceInfo info

toFieldName :: FieldName SourceInfo -> Lang.FieldName
toFieldName (FieldName fn info) =
  defMessage
    & Lang.name .~ fn
    & Lang.sourceInfo .~ toSourceInfo info

-- TODO(bladyjoker): TyAbs needs SourceInfo? Remove it if not.
toTyAbs :: [TyArg SourceInfo] -> TyBody SourceInfo -> ToProto Lang.TyAbs
toTyAbs args body = do
  body' <- toTyBody body
  return $
    defMessage
      & Lang.tyArgs .~ (toTyArg <$> args)
      & Lang.tyBody .~ body'

toTyArg :: TyArg SourceInfo -> Lang.TyArg
toTyArg (TyArg an info) =
  defMessage
    & Lang.argName . Lang.name .~ an
    & Lang.argKind . Lang.kindRef .~ Lang.Kind'KIND_REF_TYPE
    & Lang.sourceInfo .~ toSourceInfo info

toTyVar :: TyArg SourceInfo -> Lang.TyVar
toTyVar (TyArg an info) =
  defMessage
    & Lang.varName . Lang.name .~ an
    & Lang.varName . Lang.sourceInfo .~ toSourceInfo info

toTyName :: TyName SourceInfo -> Lang.TyName
toTyName (TyName n info) =
  defMessage
    & Lang.name .~ n
    & Lang.sourceInfo .~ toSourceInfo info

toClassName :: ClassName SourceInfo -> Lang.ClassName
toClassName (ClassName n info) =
  defMessage
    & Lang.name .~ n
    & Lang.sourceInfo .~ toSourceInfo info

toSourceInfo :: SourceInfo -> Lang.SourceInfo
toSourceInfo (SourceInfo f fr to) =
  defMessage
    & Lang.file .~ f
    & Lang.posFrom .~ toSourcePos fr
    & Lang.posTo .~ toSourcePos to

toSourcePos :: SourcePos -> Lang.SourcePosition
toSourcePos (SourcePos r c) =
  defMessage
    & Lang.row .~ fromIntegral r
    & Lang.column .~ fromIntegral c

toModuleName :: ModuleName SourceInfo -> Lang.ModuleName
toModuleName (ModuleName ps info) =
  defMessage
    & Lang.parts .~ (toModuleNamePart <$> ps)
    & Lang.sourceInfo .~ toSourceInfo info

toModuleNamePart :: ModuleNamePart SourceInfo -> Lang.ModuleNamePart
toModuleNamePart (ModuleNamePart p info) =
  defMessage
    & Lang.name .~ p
    & Lang.sourceInfo .~ toSourceInfo info
