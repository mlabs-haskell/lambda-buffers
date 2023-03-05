module LambdaBuffers.Codegen.Haskell.PrintM (
  runPrint,
) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExcept)
import Control.Monad.RWS (RWST (runRWST))
import Control.Monad.RWS.Class (MonadRWS, asks)
import Control.Monad.Reader.Class (MonadReader (local))
import Control.Monad.State.Class (modify)
import Control.Monad.Writer.Class (MonadWriter (tell))
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Config (Config, opaques)
import LambdaBuffers.Codegen.Haskell.Print qualified as Print
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc)
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

data PrintCtx
  = ModuleCtx PC.ModuleName
  | TyDefCtx PC.ModuleName PC.TyDef
  | InstanceClauseCtx PC.ModuleName
  deriving stock (Eq, Ord, Show)

type PrintRead = (Config, PrintCtx)

type PrintWrite = [PrintCommand]
data PrintCommand
  = AddTyDef (Doc ())
  | AddInstanceDef (Doc ())
  deriving stock (Show)

type PrintErr = String

data PrintState = MkPrintState
  { moduleLbTyImports :: Set PC.QTyName
  , moduleHsTyImports :: Set H.QTyName
  , moduleTyExports :: Set (PC.InfoLess PC.TyName)
  }
  deriving stock (Eq, Ord, Show)

type MonadPrint m = (MonadRWS PrintRead PrintWrite PrintState m, MonadError PrintErr m)

runPrint :: Config -> PC.Module -> Either P.CompilerError (Doc ())
runPrint cfg m =
  let p = runRWST (goModule m) (cfg, ModuleCtx $ m ^. #moduleName) (MkPrintState mempty mempty mempty)
      p' = runExcept p
   in go p'
  where
    go :: Either PrintErr (PC.ModuleName, PrintState, [PrintCommand]) -> Either P.CompilerError (Doc ())
    go (Right (mn, MkPrintState lti hti te, pw)) = Right $ Print.printModule mn te lti hti [tdDoc | AddTyDef tdDoc <- pw]
    go (Left printErr) = Left $ defMessage & P.internalErrors .~ [defMessage & P.msg .~ Text.pack printErr]

askConfig :: MonadPrint m => m Config
askConfig = asks fst

askCtx :: MonadPrint m => m PrintCtx
askCtx = asks snd

askTyDefCtx :: MonadPrint m => m (PC.ModuleName, PC.TyDef)
askTyDefCtx = do
  ctx <- askCtx
  case ctx of
    TyDefCtx mn td -> return (mn, td)
    other -> throwError $ "Internal error, wanted TyDefCtx got " <> show other

_askInstCtx :: MonadPrint m => m PC.ModuleName
_askInstCtx = do
  ctx <- askCtx
  case ctx of
    InstanceClauseCtx mn -> return mn
    other -> throwError $ "Internal error, wanted InstanceClauseCtx got " <> show other

exportTy :: MonadPrint m => PC.InfoLess PC.TyName -> m ()
exportTy htyN = modify (\s -> s {moduleTyExports = Set.union (moduleTyExports s) (Set.singleton htyN)})

importLbTy :: MonadPrint m => PC.QTyName -> m ()
importLbTy qtyn =
  modify
    ( \s ->
        s
          { moduleLbTyImports = Set.union (moduleLbTyImports s) (Set.singleton qtyn)
          }
    )

importHsTy :: MonadPrint m => H.QTyName -> m ()
importHsTy qtyn =
  modify
    ( \s ->
        s
          { moduleHsTyImports = Set.union (moduleHsTyImports s) (Set.singleton qtyn)
          }
    )

-- | Traverse the module and collect imports, exports and type definition documents.
goModule :: MonadPrint m => PC.Module -> m PC.ModuleName
goModule m = do
  for_ (m ^. #typeDefs) (\td -> local (\(cfg, _) -> (cfg, TyDefCtx (m ^. #moduleName) td)) (goTyDef td))
  return $ m ^. #moduleName

goTyDef :: MonadPrint m => PC.TyDef -> m ()
goTyDef td = goTyAbs $ td ^. #tyAbs

goTyAbs :: MonadPrint m => PC.TyAbs -> m ()
goTyAbs (PC.TyAbs _ (PC.OpaqueI _) _) = do
  cfg <- askConfig
  (currentModuleName, currentTyDef) <- askTyDefCtx
  let qtyn = (PC.mkInfoLess currentModuleName, PC.mkInfoLess $ currentTyDef ^. #tyName)
  qhtyn <- case Map.lookup qtyn (cfg ^. opaques) of
    Nothing -> throwError $ "TODO(bladyjoker): Opaque not configured" <> show (currentTyDef ^. #tyName)
    Just qhtyn -> return qhtyn
  exportTy (PC.mkInfoLess (currentTyDef ^. #tyName))
  importHsTy qhtyn
  tell
    [ AddTyDef $ Print.printTyDefOpaque (currentTyDef ^. #tyName) qhtyn
    ]
goTyAbs (PC.TyAbs args (PC.SumI s) _) = do
  goSum s
  currentTyDef <- snd <$> askTyDefCtx
  exportTy (PC.mkInfoLess $ currentTyDef ^. #tyName)
  tell
    [ AddTyDef $ Print.printTyDefNonOpaque (currentTyDef ^. #tyName) args (Print.Sum s)
    ]

goSum :: MonadPrint m => PC.Sum -> m ()
goSum s = for_ (s ^. #constructors) (\c -> goProduct (c ^. #product))

goProduct :: MonadPrint m => PC.Product -> m ()
goProduct (PC.TupleI t) = for_ (t ^. #fields) goTy
goProduct (PC.RecordI r) = for_ (r ^. #fields) (\f -> goTy $ f ^. #fieldTy)

goTy :: MonadPrint m => PC.Ty -> m ()
goTy (PC.TyRefI (PC.ForeignI fr)) = importLbTy (PC.mkInfoLess $ fr ^. #moduleName, PC.mkInfoLess $ fr ^. #tyName)
goTy (PC.TyAppI ta) = goTy (ta ^. #tyFunc) >> for_ (ta ^. #tyArgs) goTy
goTy _ = return ()
