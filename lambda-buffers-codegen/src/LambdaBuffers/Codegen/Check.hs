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
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.ProtoCompat qualified as PC
import Proto.Codegen qualified as P
import Proto.Codegen_Fields qualified as P

type MonadCheck qtn qcn m = (MonadRWS (CheckRead qtn qcn) () (CheckState qtn qcn) m, MonadError CheckErr m)

data CheckCtx
  = ModuleCtx PC.ModuleName
  | TyDefCtx PC.ModuleName PC.TyDef
  | ClassDefCtx PC.ModuleName
  | RuleDefCtx PC.ModuleName
  deriving stock (Eq, Ord, Show)

type CheckRead qtn qcn = (Config qtn qcn, CheckCtx)

type CheckErr = P.Error

data CheckState qtn qcn = CheckState
  { moduleTyImports :: Set PC.QTyName
  , moduleOpaqueImports :: Set qtn
  , moduleClassImports :: Set qcn
  , moduleRuleImports :: Set (PC.InfoLess PC.ModuleName)
  , moduleTyExports :: Set (PC.InfoLess PC.TyName)
  }
  deriving stock (Eq, Ord, Show)

initialCheckState :: (Ord qtn, Ord qcn) => CheckState qtn qcn
initialCheckState = CheckState mempty mempty mempty mempty mempty

runCheck :: forall qtn qcn. (Ord qtn, Ord qcn) => Config qtn qcn -> PC.CodegenInput -> PC.Module -> Either P.Error (Print.Context qtn qcn)
runCheck cfg ci m =
  let p =
        runRWST
          (checkModule m)
          (cfg, ModuleCtx $ m ^. #moduleName)
          initialCheckState
      p' = runExcept p
   in go p'
  where
    go :: Either CheckErr ((), CheckState qtn qcn, ()) -> Either P.Error (Print.Context qtn qcn)
    go (Right ((), CheckState lbtyImps opqImps clImps ruleImps tyExprts, _)) = Right $ Print.Context ci m lbtyImps opqImps clImps ruleImps tyExprts cfg
    go (Left printErr) = Left printErr

askConfig :: MonadCheck qtn qcn m => m (Config qtn qcn)
askConfig = asks fst

askCtx :: MonadCheck qtn qcn m => m CheckCtx
askCtx = asks snd

throwInternalError :: MonadCheck qtn qcn m => String -> m a
throwInternalError msg = throwError $ defMessage & P.internalErrors .~ [defMessage & P.msg .~ "[LambdaBuffers.Codegen.Check] " <> Text.pack msg]

throwUnsupportedOpaqueError :: MonadCheck qtn qcn m => PC.ModuleName -> PC.TyName -> m a
throwUnsupportedOpaqueError mn tyN =
  throwError $
    defMessage
      & P.unsupportedOpaqueErrors
        .~ [ defMessage
              & P.moduleName .~ PC.toProto mn
              & P.tyName .~ PC.toProto tyN
           ]

throwUnsupportedClassError :: MonadCheck qtn qcn m => PC.ModuleName -> PC.ClassName -> m a
throwUnsupportedClassError mn clN =
  throwError $
    defMessage
      & P.unsupportedClassErrors
        .~ [ defMessage
              & P.moduleName .~ PC.toProto mn
              & P.className .~ PC.toProto clN
           ]

askTyDefCtx :: MonadCheck qtn qcn m => m (PC.ModuleName, PC.TyDef)
askTyDefCtx = do
  ctx <- askCtx
  case ctx of
    TyDefCtx mn td -> return (mn, td)
    other -> throwInternalError $ "Wanted TyDefCtx got " <> show other

askClassDefCtx :: MonadCheck qtn qcn m => m PC.ModuleName
askClassDefCtx = do
  ctx <- askCtx
  case ctx of
    ClassDefCtx mn -> return mn
    other -> throwInternalError $ "Wanted ClassDefCtx got " <> show other

exportTy :: MonadCheck qtn qcn m => PC.InfoLess PC.TyName -> m ()
exportTy htyN = modify (\s -> s {moduleTyExports = Set.union (moduleTyExports s) (Set.singleton htyN)})

importTy :: MonadCheck qtn qcn m => PC.QTyName -> m ()
importTy qtyn = modify (\s -> s {moduleTyImports = Set.union (moduleTyImports s) (Set.singleton qtyn)})

importOpaqueTy :: Ord qtn => MonadCheck qtn qcn m => qtn -> m ()
importOpaqueTy qtyn = modify (\s -> s {moduleOpaqueImports = Set.union (moduleOpaqueImports s) (Set.singleton qtyn)})

importClass :: (MonadCheck qtn qcn m, Ord qcn) => qcn -> m ()
importClass qcn = modify (\s -> s {moduleClassImports = Set.union (moduleClassImports s) (Set.singleton qcn)})

importRulesFrom :: MonadCheck qtn qcn m => PC.InfoLess PC.ModuleName -> m ()
importRulesFrom mn = modify (\s -> s {moduleRuleImports = Set.union (moduleRuleImports s) (Set.singleton mn)})

-- | Traverse the module and collect imports and exports, erroring if anything is not configured.
checkModule :: (MonadCheck qtn qcn m, Ord qtn, Ord qcn) => PC.Module -> m ()
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

checkTyDef :: (MonadCheck qtn qcn m, Ord qtn) => PC.TyDef -> m ()
checkTyDef td = do
  checkTyAbs $ td ^. #tyAbs
  exportTy (PC.mkInfoLess $ td ^. #tyName)

checkTyAbs :: (MonadCheck qtn qcn m, Ord qtn) => PC.TyAbs -> m ()
checkTyAbs (PC.TyAbs _ body _) = checkTyBody body

checkTyBody :: (MonadCheck qtn qcn m, Ord qtn) => PC.TyBody -> m ()
checkTyBody (PC.SumI s) = checkSum s
checkTyBody (PC.ProductI p) = checkProduct p
checkTyBody (PC.RecordI r) = checkRecord r
checkTyBody (PC.OpaqueI _) = checkOpaque

checkOpaque :: (MonadCheck qtn qcn m, Ord qtn) => m ()
checkOpaque = do
  cfg <- askConfig
  (currentModuleName, currentTyDef) <- askTyDefCtx
  let qtyn = (PC.mkInfoLess currentModuleName, PC.mkInfoLess $ currentTyDef ^. #tyName)
  qotyn <- case Map.lookup qtyn (cfg ^. cfgOpaques) of
    Nothing -> throwUnsupportedOpaqueError currentModuleName (currentTyDef ^. #tyName)
    Just qhtyn -> return qhtyn
  importOpaqueTy qotyn

checkSum :: MonadCheck qtn qcn m => PC.Sum -> m ()
checkSum s = for_ (s ^. #constructors) (\c -> checkProduct (c ^. #product))

checkProduct :: MonadCheck qtn qcn m => PC.Product -> m ()
checkProduct p = for_ (p ^. #fields) checkTy

checkRecord :: MonadCheck qtn qcn m => PC.Record -> m ()
checkRecord r = for_ (r ^. #fields) (\f -> checkTy $ f ^. #fieldTy)

checkTy :: MonadCheck qtn qcn m => PC.Ty -> m ()
checkTy (PC.TyRefI (PC.ForeignI fr)) = importTy (PC.mkInfoLess $ fr ^. #moduleName, PC.mkInfoLess $ fr ^. #tyName)
checkTy (PC.TyAppI ta) = checkTy (ta ^. #tyFunc) >> for_ (ta ^. #tyArgs) checkTy
checkTy _ = return ()

-- TODO(bladyjoker): This is where you lookup instance implementation and report if an instance implementation is missing.
checkInstanceClause :: (MonadCheck qtn qcn m) => PC.InstanceClause -> m ()
checkInstanceClause ic = do
  checkConstraint $ ic ^. #head
  for_ (ic ^. #constraints) checkConstraint

checkDerive :: (MonadCheck qtn qcn m) => PC.Derive -> m ()
checkDerive drv = checkConstraint $ drv ^. #constraint

checkConstraint :: (MonadCheck qtn qcn m) => PC.Constraint -> m ()
checkConstraint c = do
  checkTy $ c ^. #argument

checkClassDef :: (MonadCheck qtn qcn m, Ord qcn) => PC.ClassDef -> m ()
checkClassDef cd = do
  cfg <- askConfig
  mn <- askClassDefCtx
  let qcn = PC.qualifyClassName mn (cd ^. #className)
  case Map.lookup qcn (cfg ^. cfgClasses) of
    Nothing -> throwUnsupportedClassError mn (cd ^. #className)
    Just clImps -> for_ clImps importClass
