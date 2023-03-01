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
import LambdaBuffers.Compiler.ProtoCompat.Types (Module, TyAbs (TyAbs), TyBody (OpaqueI, SumI), TyDef)
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
  { moduleTyImports :: Set H.QTyName
  , moduleTyExports :: Set H.TyName
  , moduleClassImports :: Set (H.QClassName, [H.FunctionName])
  }
  deriving stock (Eq, Ord, Show)

type MonadPrint m = (MonadRWS PrintRead PrintWrite PrintState m, MonadError PrintErr m)

runPrint :: Config -> Module -> Either P.CompilerError (Doc ())
runPrint cfg m =
  let p = runRWST (goModule m) (cfg, ModuleCtx $ m ^. #moduleName) (MkPrintState mempty mempty mempty)
      p' = runExcept p
   in go p'
  where
    go :: Either PrintErr (H.ModuleName, PrintState, [PrintCommand]) -> Either P.CompilerError (Doc ())
    go (Right (mn, MkPrintState ti te _, pw)) = Right $ Print.printModule mn te ti [tdDoc | AddTyDef tdDoc <- pw]
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

exportTy :: MonadPrint m => H.TyName -> m ()
exportTy htyN = modify (\s -> s {moduleTyExports = Set.union (moduleTyExports s) (Set.singleton htyN)})

importTy :: MonadPrint m => (H.CabalPackageName, H.ModuleName, H.TyName) -> m ()
importTy qhTyRef = modify (\s -> s {moduleTyImports = Set.union (moduleTyImports s) (Set.singleton qhTyRef)})

_importClass :: MonadPrint m => (H.QClassName, [H.FunctionName]) -> m ()
_importClass qhClassRef = modify (\s -> s {moduleClassImports = Set.union (moduleClassImports s) (Set.singleton qhClassRef)})

-- | Traverse the module and collect imports, exports and type definition documents.
goModule :: MonadPrint m => Module -> m H.ModuleName
goModule m = do
  for_ (m ^. #typeDefs) (\td -> local (\(cfg, _) -> (cfg, TyDefCtx (m ^. #moduleName) td)) (goTyDef td))
  return $ H.fromLbModuleName (m ^. #moduleName)

goTyDef :: MonadPrint m => TyDef -> m ()
goTyDef td = goTyAbs $ td ^. #tyAbs

goTyAbs :: MonadPrint m => TyAbs -> m ()
goTyAbs (TyAbs _ (OpaqueI _) _) = do
  cfg <- askConfig
  (currentModuleName, currentTyDef) <- askTyDefCtx
  -- FIXME(bladyjoker): Must be infoLess.
  qhTyRef <- case Map.lookup (currentModuleName, currentTyDef ^. #tyName) (cfg ^. opaques) of
    Nothing -> throwError $ "TODO(bladyjoker): Opaque not configured" <> show (currentTyDef ^. #tyName)
    Just qhsTyRef -> return qhsTyRef
  exportTy (H.fromLbTyName (currentTyDef ^. #tyName))
  importTy qhTyRef
  tell
    [ AddTyDef $ Print.printTyDefOpaque (currentTyDef ^. #tyName) qhTyRef
    ]
goTyAbs (TyAbs args (SumI s) _) = do
  currentTyDef <- snd <$> askTyDefCtx
  exportTy (H.fromLbTyName (currentTyDef ^. #tyName))
  tell
    [ AddTyDef $ Print.printTyDefNonOpaque (currentTyDef ^. #tyName) args (Print.Sum s)
    ]

_foreignTyRefToHaskImport :: PC.ForeignRef -> (H.CabalPackageName, H.ModuleName, H.TyName)
_foreignTyRefToHaskImport fr =
  ( H.cabalFromLbModuleName $ fr ^. #moduleName
  , H.fromLbModuleName $ fr ^. #moduleName
  , H.fromLbTyName $ fr ^. #tyName
  )
