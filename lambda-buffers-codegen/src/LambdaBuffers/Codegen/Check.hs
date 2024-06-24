module LambdaBuffers.Codegen.Check (
  runCheck,
) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExcept)
import Control.Monad.RWS (RWST (runRWST))
import Control.Monad.RWS.Class (MonadRWS, asks)
import Control.Monad.Reader.Class (MonadReader (local))
import Control.Monad.State.Class (modify)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Config (Config, cfgClasses, cfgOpaques)
import LambdaBuffers.Codegen.Print (IsBackend (BackendContext, BackendQualifiedClassName, BackendQualifiedTyName))
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.ProtoCompat qualified as PC
import Proto.Codegen qualified as P
import Proto.Codegen_Fields qualified as P

type MonadCheck backend m = (IsBackend backend, MonadRWS (CheckRead backend) () (CheckState backend) m, MonadError CheckErr m)

data CheckCtx
  = ModuleCtx PC.ModuleName
  | TyDefCtx PC.ModuleName PC.TyDef
  | ClassDefCtx PC.ModuleName
  | RuleDefCtx PC.ModuleName
  deriving stock (Eq, Ord, Show)

type CheckRead backend = (Config (BackendQualifiedTyName backend) (BackendQualifiedClassName backend), CheckCtx)

type CheckErr = P.Error

data CheckState backend = CheckState
  { moduleTyImports :: Set PC.QTyName
  , moduleOpaqueImports :: Set (BackendQualifiedTyName backend)
  , moduleClassImports :: Set (BackendQualifiedClassName backend)
  , moduleRuleImports :: Set (PC.InfoLess PC.ModuleName)
  , moduleTyExports :: Set (PC.InfoLess PC.TyName)
  }

initialCheckState :: IsBackend backend => CheckState backend
initialCheckState = CheckState mempty mempty mempty mempty mempty

runCheck ::
  forall backend.
  IsBackend backend =>
  Config (BackendQualifiedTyName backend) (BackendQualifiedClassName backend) ->
  BackendContext backend ->
  PC.CodegenInput ->
  PC.Module ->
  Either P.Error (Print.Context backend)
runCheck cfg bctx ci m =
  let p =
        runRWST
          (checkModule m)
          (cfg, ModuleCtx $ m ^. #moduleName)
          initialCheckState
      p' = runExcept p
   in go p'
  where
    go :: Either CheckErr ((), CheckState backend, ()) -> Either P.Error (Print.Context backend)
    go (Right ((), CheckState lbtyImps opqImps clImps ruleImps tyExprts, _)) =
      Right $
        Print.Context @backend
          ci
          m
          lbtyImps
          opqImps
          clImps
          ruleImps
          tyExprts
          cfg
          bctx
    go (Left printErr) = Left printErr

askConfig :: MonadCheck backend m => m (Config (BackendQualifiedTyName backend) (BackendQualifiedClassName backend))
askConfig = asks fst

askCtx :: MonadCheck backend m => m CheckCtx
askCtx = asks snd

throwInternalError :: MonadCheck backend m => String -> m a
throwInternalError msg = throwError $ defMessage & P.internalErrors .~ [defMessage & P.msg .~ "[LambdaBuffers.Codegen.Check] " <> Text.pack msg]

throwUnsupportedOpaqueError :: MonadCheck backend m => PC.ModuleName -> PC.TyName -> m a
throwUnsupportedOpaqueError mn tyN =
  throwError $
    defMessage
      & P.unsupportedOpaqueErrors
        .~ [ defMessage
              & P.moduleName .~ PC.toProto mn
              & P.tyName .~ PC.toProto tyN
           ]

throwUnsupportedClassError :: MonadCheck backend m => PC.ModuleName -> PC.ClassName -> m a
throwUnsupportedClassError mn clN =
  throwError $
    defMessage
      & P.unsupportedClassErrors
        .~ [ defMessage
              & P.moduleName .~ PC.toProto mn
              & P.className .~ PC.toProto clN
           ]

askTyDefCtx :: MonadCheck backend m => m (PC.ModuleName, PC.TyDef)
askTyDefCtx = do
  ctx <- askCtx
  case ctx of
    TyDefCtx mn td -> return (mn, td)
    other -> throwInternalError $ "Wanted TyDefCtx got " <> show other

askClassDefCtx :: MonadCheck backend m => m PC.ModuleName
askClassDefCtx = do
  ctx <- askCtx
  case ctx of
    ClassDefCtx mn -> return mn
    other -> throwInternalError $ "Wanted ClassDefCtx got " <> show other

exportTy :: MonadCheck backend m => PC.InfoLess PC.TyName -> m ()
exportTy htyN = modify (\s -> s {moduleTyExports = Set.union (moduleTyExports s) (Set.singleton htyN)})

importTy :: MonadCheck backend m => PC.QTyName -> m ()
importTy qtyn = modify (\s -> s {moduleTyImports = Set.union (moduleTyImports s) (Set.singleton qtyn)})

importOpaqueTy :: MonadCheck backend m => BackendQualifiedTyName backend -> m ()
importOpaqueTy qtyn = modify (\s -> s {moduleOpaqueImports = Set.union (moduleOpaqueImports s) (Set.singleton qtyn)})

importClass :: MonadCheck backend m => BackendQualifiedClassName backend -> m ()
importClass qcn = modify (\s -> s {moduleClassImports = Set.union (moduleClassImports s) (Set.singleton qcn)})

importRulesFrom :: MonadCheck backend m => PC.InfoLess PC.ModuleName -> m ()
importRulesFrom mn = modify (\s -> s {moduleRuleImports = Set.union (moduleRuleImports s) (Set.singleton mn)})

-- | Traverse the module and collect imports and exports, erroring if anything is not configured.
checkModule :: MonadCheck backend m => PC.Module -> m ()
checkModule m = do
  for_
    (m ^. #typeDefs)
    (\td -> local (\(cfg, _) -> (cfg, TyDefCtx (m ^. #moduleName) td)) (checkTyDef td))
  for_
    (m ^. #classDefs)
    (local (\(cfg, _) -> (cfg, ClassDefCtx (m ^. #moduleName))) . checkClassDef)
  for_
    (m ^. #instances)
    (local (\(cfg, _) -> (cfg, RuleDefCtx (m ^. #moduleName))) . checkInstanceClause)
  for_
    (m ^. #derives)
    (local (\(cfg, _) -> (cfg, RuleDefCtx (m ^. #moduleName))) . checkDerive)
  for_
    (Map.keys $ m ^. #imports)
    importRulesFrom

checkTyDef :: MonadCheck backend m => PC.TyDef -> m ()
checkTyDef td = do
  checkTyAbs $ td ^. #tyAbs
  exportTy (PC.mkInfoLess $ td ^. #tyName)

checkTyAbs :: MonadCheck backend m => PC.TyAbs -> m ()
checkTyAbs (PC.TyAbs _ body _) = checkTyBody body

checkTyBody :: MonadCheck backend m => PC.TyBody -> m ()
checkTyBody (PC.SumI s) = checkSum s
checkTyBody (PC.ProductI p) = checkProduct p
checkTyBody (PC.RecordI r) = checkRecord r
checkTyBody (PC.OpaqueI _) = checkOpaque

checkOpaque :: MonadCheck backend m => m ()
checkOpaque = do
  cfg <- askConfig
  (currentModuleName, currentTyDef) <- askTyDefCtx
  let qtyn = (PC.mkInfoLess currentModuleName, PC.mkInfoLess $ currentTyDef ^. #tyName)
  qotyn <- case Map.lookup qtyn (cfg ^. cfgOpaques) of
    Nothing -> throwUnsupportedOpaqueError currentModuleName (currentTyDef ^. #tyName)
    Just qhtyn -> return qhtyn
  importOpaqueTy qotyn

checkSum :: MonadCheck backend m => PC.Sum -> m ()
checkSum s = for_ (s ^. #constructors) (\c -> checkProduct (c ^. #product))

checkProduct :: MonadCheck backend m => PC.Product -> m ()
checkProduct p = for_ (p ^. #fields) checkTy

checkRecord :: MonadCheck backend m => PC.Record -> m ()
checkRecord r = for_ (r ^. #fields) (\f -> checkTy $ f ^. #fieldTy)

checkTy :: MonadCheck backend m => PC.Ty -> m ()
checkTy (PC.TyRefI (PC.ForeignI fr)) = importTy (PC.mkInfoLess $ fr ^. #moduleName, PC.mkInfoLess $ fr ^. #tyName)
checkTy (PC.TyAppI ta) = checkTy (ta ^. #tyFunc) >> for_ (ta ^. #tyArgs) checkTy
checkTy _ = return ()

-- TODO(bladyjoker): This is where you should lookup instance implementation and report if an instance implementation is missing.
checkInstanceClause :: MonadCheck backend m => PC.InstanceClause -> m ()
checkInstanceClause ic = do
  checkConstraint $ ic ^. #head
  for_ (ic ^. #constraints) checkConstraint

checkDerive :: MonadCheck backend m => PC.Derive -> m ()
checkDerive drv = checkConstraint $ drv ^. #constraint

checkConstraint :: MonadCheck backend m => PC.Constraint -> m ()
checkConstraint c = do
  checkTy $ c ^. #argument

checkClassDef :: MonadCheck backend m => PC.ClassDef -> m ()
checkClassDef cd = do
  cfg <- askConfig
  mn <- askClassDefCtx
  let qcn = PC.qualifyClassName mn (cd ^. #className)
  case Map.lookup qcn (cfg ^. cfgClasses) of
    Nothing -> throwUnsupportedClassError mn (cd ^. #className)
    Just clImps -> for_ clImps importClass
