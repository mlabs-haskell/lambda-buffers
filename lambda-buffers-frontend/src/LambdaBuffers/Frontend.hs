module LambdaBuffers.Frontend (runFrontend, FrontendError (..), FrontendResult (..), parseModule) where

import Control.Monad (foldM, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (MonadIO (liftIO), MonadTrans (lift), StateT (runStateT), gets, modify)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, local)
import Data.List (isSuffixOf)
import Data.Map qualified as Map
import Data.Text (Text, unpack)
import Data.Text.IO qualified as Text
import LambdaBuffers.Frontend.CheckReferences (checkReferences)
import LambdaBuffers.Frontend.Errors (FrontendError)
import LambdaBuffers.Frontend.Errors qualified as Errors
import LambdaBuffers.Frontend.Monad (FrontRead (FrontRead, current, importPaths, visited), FrontState (FrontState, importedModules), FrontendResult (FrontendResult), FrontendT, throwE')
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Parsec qualified as Parsec
import LambdaBuffers.Frontend.Scope (addToClassScope, addToTyScope, collectImportedScope, collectLocalScope)
import LambdaBuffers.Frontend.Syntax (Import (importModuleName), Module (moduleImports, moduleName), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), SourceInfo, defSourceInfo)
import LambdaBuffers.Frontend.Utils (ClassScope, TyScope, strip)
import System.Directory (findFiles)
import System.FilePath (joinPath, (<.>))

-- | Run a Frontend compilation action on a "lbf" file, return the entire compilation closure or a frontend error.
runFrontend :: MonadIO m => [FilePath] -> FilePath -> m (Either FrontendError FrontendResult)
runFrontend importPaths modFp = do
  let stM = runReaderT (processFile modFp) (FrontRead (ModuleName [] defSourceInfo) [] importPaths)
      exM = runStateT stM (FrontState mempty)
      ioM = runExceptT exM
  fmap (FrontendResult . importedModules . snd) <$> ioM

moduleNameToFilepath :: ModuleName info -> FilePath
moduleNameToFilepath (ModuleName parts _) = joinPath [unpack p | ModuleNamePart p _ <- parts] <.> "lbf"

checkCycle :: Import SourceInfo -> FrontendT m ()
checkCycle imp = do
  ms <- asks visited
  cm <- asks current
  when ((strip . importModuleName $ imp) `elem` ms) $ throwE' $ Errors.ImportCycleFound cm imp ms

-- | `parseModule fp txt` parses a LambdaBuffers module with a specified filename `fp` (for reporting) and content in `txt`.
parseModule :: FilePath -> Text -> FrontendT m (Module SourceInfo)
parseModule modFp modContent = do
  modOrErr <- liftIO $ Parsec.runParser Parsec.parseModule modFp modContent
  case modOrErr of
    Left err -> throwE' $ Errors.ModuleParseError modFp err
    Right m -> return m

importModule :: Import SourceInfo -> FrontendT m (Module SourceInfo)
importModule imp = do
  let modName = importModuleName imp
  ims <- gets importedModules
  case Map.lookup (strip modName) ims of
    Nothing -> do
      ips <- asks importPaths
      found <- liftIO $ findFiles ips (moduleNameToFilepath modName)
      case found of
        [] -> do
          cm <- asks current
          throwE' $ Errors.ModuleNotFound cm imp ips
        [modFp] -> do
          modContent <- liftIO $ Text.readFile modFp
          modOrErr <- liftIO $ Parsec.runParser Parsec.parseModule modFp modContent
          case modOrErr of
            Left err -> throwE' $ Errors.ModuleParseError modFp err
            Right m -> return m
        modFps -> do
          cm <- asks current
          throwE' $ Errors.MultipleModulesFound cm imp modFps
    Just (m, _) -> return m

processFile :: FilePath -> FrontendT m (Module SourceInfo)
processFile modFp = do
  modContent <- liftIO $ Text.readFile modFp
  m <- parseModule modFp modContent
  checkModuleName modFp (moduleName m)
  processModule m

processModule :: Module SourceInfo -> FrontendT m (Module SourceInfo)
processModule m = local
  ( \ir ->
      ir
        { current = moduleName m
        , visited = (strip . current $ ir) : visited ir
        }
  )
  $ do
    importedScope <- processImports m
    localScope <- collectLocalScope m importedScope
    let modScope = localScope <> importedScope
    checkReferences m modScope
    _ <- lift $ modify (FrontState . Map.insert (strip . moduleName $ m) (m, modScope) . importedModules)
    return m

checkModuleName :: FilePath -> ModuleName SourceInfo -> FrontendT m ()
checkModuleName fp mn =
  let suffix = moduleNameToFilepath mn
   in if suffix `isSuffixOf` fp
        then return ()
        else throwE' $ Errors.InvalidModuleFilepath mn fp suffix

processImports :: Module SourceInfo -> FrontendT m (TyScope, ClassScope)
processImports m = do
  (totalTyScope, totalClassScope) <-
    foldM
      ( \(totalTyScope', totalClassScope') imp ->
          do
            (tyScope, classScope) <- processImport imp
            (,)
              <$> addToTyScope totalTyScope' (imp, tyScope)
              <*> addToClassScope totalClassScope' (imp, classScope)
      )
      mempty
      (moduleImports m)
  return (totalTyScope, totalClassScope)

processImport :: Import SourceInfo -> FrontendT m (TyScope, ClassScope)
processImport imp = do
  checkCycle imp
  im <- importModule imp >>= processModule
  collectImportedScope imp im
