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
import LambdaBuffers.Codegen.Config (Config, opaques)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

type MonadCheck o c m = (MonadRWS (CheckRead o c) () (CheckState o) m, MonadError CheckErr m)

data CheckCtx
  = ModuleCtx PC.ModuleName
  | TyDefCtx PC.ModuleName PC.TyDef
  | InstanceClauseCtx PC.ModuleName
  deriving stock (Eq, Ord, Show)

type CheckRead o c = (Config o c, CheckCtx)

type CheckErr = String

data CheckState o = MkCheckState
  { moduleLbTyImports :: Set PC.QTyName
  , moduleOpaqueImports :: Set o
  , moduleTyExports :: Set (PC.InfoLess PC.TyName)
  }
  deriving stock (Eq, Ord, Show)

runCheck :: forall o c. Ord o => Config o c -> PC.Module -> Either P.CompilerError (Print.Context o c)
runCheck cfg m =
  let p = runRWST (checkModule m) (cfg, ModuleCtx $ m ^. #moduleName) (MkCheckState mempty (mempty :: Set o) mempty)
      p' = runExcept p
   in go p'
  where
    go :: Either CheckErr ((), CheckState o, ()) -> Either P.CompilerError (Print.Context o c)
    go (Right ((), MkCheckState lti oti te, _)) = Right $ Print.MkContext m lti oti te cfg
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

_askInstCtx :: MonadCheck o c m => m PC.ModuleName
_askInstCtx = do
  ctx <- askCtx
  case ctx of
    InstanceClauseCtx mn -> return mn
    other -> throwError $ "Internal error, wanted InstanceClauseCtx got " <> show other

exportTy :: MonadCheck o c m => PC.InfoLess PC.TyName -> m ()
exportTy htyN = modify (\s -> s {moduleTyExports = Set.union (moduleTyExports s) (Set.singleton htyN)})

importLbTy :: MonadCheck o c m => PC.QTyName -> m ()
importLbTy qtyn = modify (\s -> s {moduleLbTyImports = Set.union (moduleLbTyImports s) (Set.singleton qtyn)})

importOpaqueTy :: Ord o => MonadCheck o c m => o -> m ()
importOpaqueTy qtyn = modify (\s -> s {moduleOpaqueImports = Set.union (moduleOpaqueImports s) (Set.singleton qtyn)})

-- | Traverse the module and collect imports and exports.
checkModule :: (MonadCheck o c m, Ord o) => PC.Module -> m ()
checkModule m =
  for_
    (m ^. #typeDefs)
    (\td -> local (\(cfg, _) -> (cfg, TyDefCtx (m ^. #moduleName) td)) (checkTyDef td))

checkTyDef :: (MonadCheck o c m, Ord o) => PC.TyDef -> m ()
checkTyDef td = checkTyAbs $ td ^. #tyAbs

checkTyAbs :: (MonadCheck o c m, Ord o) => PC.TyAbs -> m ()
checkTyAbs (PC.TyAbs _ (PC.OpaqueI _) _) = do
  cfg <- askConfig
  (currentModuleName, currentTyDef) <- askTyDefCtx
  let qtyn = (PC.mkInfoLess currentModuleName, PC.mkInfoLess $ currentTyDef ^. #tyName)
  qotyn <- case Map.lookup qtyn (cfg ^. opaques) of
    Nothing -> throwError $ "TODO(bladyjoker): Opaque not configured " <> show (currentTyDef ^. #tyName)
    Just qhtyn -> return qhtyn
  exportTy (PC.mkInfoLess (currentTyDef ^. #tyName))
  importOpaqueTy qotyn
checkTyAbs (PC.TyAbs _ (PC.SumI s) _) = do
  checkSum s
  currentTyDef <- snd <$> askTyDefCtx
  exportTy (PC.mkInfoLess $ currentTyDef ^. #tyName)

checkSum :: MonadCheck o c m => PC.Sum -> m ()
checkSum s = for_ (s ^. #constructors) (\c -> checkProduct (c ^. #product))

checkProduct :: MonadCheck o c m => PC.Product -> m ()
checkProduct (PC.TupleI t) = for_ (t ^. #fields) checkTy
checkProduct (PC.RecordI r) = for_ (r ^. #fields) (\f -> checkTy $ f ^. #fieldTy)

checkTy :: MonadCheck o c m => PC.Ty -> m ()
checkTy (PC.TyRefI (PC.ForeignI fr)) = importLbTy (PC.mkInfoLess $ fr ^. #moduleName, PC.mkInfoLess $ fr ^. #tyName)
checkTy (PC.TyAppI ta) = checkTy (ta ^. #tyFunc) >> for_ (ta ^. #tyArgs) checkTy
checkTy _ = return ()
