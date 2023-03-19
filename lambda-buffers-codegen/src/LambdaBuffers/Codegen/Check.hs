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
import LambdaBuffers.Codegen.Config (Config, classes, opaques)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

type MonadCheck o c m = (MonadRWS (CheckRead o c) () (CheckState o c) m, MonadError CheckErr m)

data CheckCtx
  = ModuleCtx PC.ModuleName
  | TyDefCtx PC.ModuleName PC.TyDef
  | RuleDefCtx PC.ModuleName
  deriving stock (Eq, Ord, Show)

type CheckRead o c = (Config o c, CheckCtx)

type CheckErr = String

data CheckState o c = MkCheckState
  { moduleTyImports :: Set PC.QTyName
  , moduleOpaqueImports :: Set o
  , moduleClassImports :: Set c
  , moduleRuleImports :: Set (PC.InfoLess PC.ModuleName)
  , moduleTyExports :: Set (PC.InfoLess PC.TyName)
  }
  deriving stock (Eq, Ord, Show)

initialCheckState :: (Ord o, Ord c) => CheckState o c
initialCheckState = MkCheckState mempty mempty mempty mempty mempty

runCheck :: forall o c. (Ord o, Ord c) => Config o c -> PC.CompilerInput -> PC.Module -> Either P.CompilerError (Print.Context o c)
runCheck cfg ci m =
  let p =
        runRWST
          (checkModule m)
          (cfg, ModuleCtx $ m ^. #moduleName)
          initialCheckState
      p' = runExcept p
   in go p'
  where
    go :: Either CheckErr ((), CheckState o c, ()) -> Either P.CompilerError (Print.Context o c)
    go (Right ((), MkCheckState lbtyImps opqImps clImps ruleImps tyExprts, _)) = Right $ Print.MkContext ci m lbtyImps opqImps clImps ruleImps tyExprts cfg
    go (Left printErr) = Left $ defMessage & P.internalErrors .~ [defMessage & P.msg .~ Text.pack printErr]

askConfig :: MonadCheck o c m => m (Config o c)
askConfig = asks fst

askCtx :: MonadCheck o c m => m CheckCtx
askCtx = asks snd

askTyDefCtx :: MonadCheck o c m => m (PC.ModuleName, PC.TyDef)
askTyDefCtx = do
  ctx <- askCtx
  case ctx of
    TyDefCtx mn td -> return (mn, td)
    other -> throwError $ "Internal error, wanted TyDefCtx got " <> show other

askInstCtx :: MonadCheck o c m => m PC.ModuleName
askInstCtx = do
  ctx <- askCtx
  case ctx of
    RuleDefCtx mn -> return mn
    other -> throwError $ "Internal error, wanted RuleDefCtx got " <> show other

exportTy :: MonadCheck o c m => PC.InfoLess PC.TyName -> m ()
exportTy htyN = modify (\s -> s {moduleTyExports = Set.union (moduleTyExports s) (Set.singleton htyN)})

importTy :: MonadCheck o c m => PC.QTyName -> m ()
importTy qtyn = modify (\s -> s {moduleTyImports = Set.union (moduleTyImports s) (Set.singleton qtyn)})

importOpaqueTy :: Ord o => MonadCheck o c m => o -> m ()
importOpaqueTy qtyn = modify (\s -> s {moduleOpaqueImports = Set.union (moduleOpaqueImports s) (Set.singleton qtyn)})

importClass :: (MonadCheck o c m, Ord c) => c -> m ()
importClass qcn = modify (\s -> s {moduleClassImports = Set.union (moduleClassImports s) (Set.singleton qcn)})

importRulesFrom :: MonadCheck o c m => PC.InfoLess PC.ModuleName -> m ()
importRulesFrom mn = modify (\s -> s {moduleRuleImports = Set.union (moduleRuleImports s) (Set.singleton mn)})

-- | Traverse the module and collect imports and exports, erroring if anything is not configured.
checkModule :: (MonadCheck o c m, Ord o, Ord c) => PC.Module -> m ()
checkModule m = do
  for_
    (m ^. #typeDefs)
    (\td -> local (\(cfg, _) -> (cfg, TyDefCtx (m ^. #moduleName) td)) (checkTyDef td))

  for_
    (m ^. #instances)
    (local (\(cfg, _) -> (cfg, RuleDefCtx (m ^. #moduleName))) . checkInstanceClause)

  for_
    (m ^. #derives)
    (local (\(cfg, _) -> (cfg, RuleDefCtx (m ^. #moduleName))) . checkDerive)

  for_
    (Map.keys $ m ^. #imports)
    importRulesFrom

checkTyDef :: (MonadCheck o c m, Ord o) => PC.TyDef -> m ()
checkTyDef td = do
  checkTyAbs $ td ^. #tyAbs
  exportTy (PC.mkInfoLess $ td ^. #tyName)

checkTyAbs :: (MonadCheck o c m, Ord o) => PC.TyAbs -> m ()
checkTyAbs (PC.TyAbs _ body _) = checkTyBody body

checkTyBody :: (MonadCheck o c m, Ord o) => PC.TyBody -> m ()
checkTyBody (PC.SumI s) = checkSum s
checkTyBody (PC.ProductI p) = checkProduct p
checkTyBody (PC.RecordI r) = checkRecord r
checkTyBody (PC.OpaqueI _) = checkOpaque

checkOpaque :: (MonadCheck o c m, Ord o) => m ()
checkOpaque = do
  cfg <- askConfig
  (currentModuleName, currentTyDef) <- askTyDefCtx
  let qtyn = (PC.mkInfoLess currentModuleName, PC.mkInfoLess $ currentTyDef ^. #tyName)
  qotyn <- case Map.lookup qtyn (cfg ^. opaques) of
    Nothing -> throwError $ "TODO(bladyjoker): Opaque not configured " <> show (currentTyDef ^. #tyName)
    Just qhtyn -> return qhtyn
  importOpaqueTy qotyn

checkSum :: MonadCheck o c m => PC.Sum -> m ()
checkSum s = for_ (s ^. #constructors) (\c -> checkProduct (c ^. #product))

checkProduct :: MonadCheck o c m => PC.Product -> m ()
checkProduct p = for_ (p ^. #fields) checkTy

checkRecord :: MonadCheck o c m => PC.Record -> m ()
checkRecord r = for_ (r ^. #fields) (\f -> checkTy $ f ^. #fieldTy)

checkTy :: MonadCheck o c m => PC.Ty -> m ()
checkTy (PC.TyRefI (PC.ForeignI fr)) = importTy (PC.mkInfoLess $ fr ^. #moduleName, PC.mkInfoLess $ fr ^. #tyName)
checkTy (PC.TyAppI ta) = checkTy (ta ^. #tyFunc) >> for_ (ta ^. #tyArgs) checkTy
checkTy _ = return ()

-- TODO(bladyjoker): This is where you lookup instance implementation and report if an instance implementation is missing.
checkInstanceClause :: (MonadCheck o c m, Ord c) => PC.InstanceClause -> m ()
checkInstanceClause ic = do
  checkConstraint $ ic ^. #head
  for_ (ic ^. #constraints) checkConstraint

checkDerive :: (MonadCheck o c m, Ord c) => PC.Derive -> m ()
checkDerive drv = checkConstraint $ drv ^. #constraint

checkConstraint :: (MonadCheck o c m, Ord c) => PC.Constraint -> m ()
checkConstraint c = do
  resolveClassRef (c ^. #classRef)
  checkTy $ c ^. #argument

resolveClassRef :: (MonadCheck o c m, Ord c) => PC.TyClassRef -> m ()
resolveClassRef cr = do
  cfg <- askConfig
  mn <- askInstCtx
  let qcn = PC.qualifyClassRef mn cr
  case Map.lookup qcn (cfg ^. classes) of
    Nothing -> throwError $ "TODO(bladyjoker): Class not configured " <> show cr
    Just clImp -> importClass clImp
