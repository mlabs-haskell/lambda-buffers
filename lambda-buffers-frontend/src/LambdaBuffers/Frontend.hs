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
import LambdaBuffers.Frontend.Monad (FrontRead (FrontRead, current, importPaths, visited), FrontState (FrontState, fstate'modules), FrontendResult (FrontendResult), FrontendT, throwE')
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Parsec qualified as Parsec
import LambdaBuffers.Frontend.Scope (addToClassScope, addToTyScope, collectImportedScope, collectLocalScope)
import LambdaBuffers.Frontend.Syntax (Import (importModuleName), Module (moduleImports, moduleName), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), SourceInfo, defSourceInfo)
import LambdaBuffers.Frontend.Utils (ClassScope, TyScope, strip)
import System.Directory (findFiles)
import System.FilePath (joinPath, (<.>))

-- | `runFrontend importPaths modFps` processes all .lbf schemas specified in `modFps` files with `importPaths` directories. Returns modules that represent the entire compilation closure or a Frontend error.
runFrontend :: MonadIO m => [FilePath] -> [FilePath] -> m (Either FrontendError FrontendResult)
runFrontend importPaths modFps = do
  let stM = runReaderT (processSchemas modFps) (FrontRead (ModuleName [] defSourceInfo) [] importPaths)
      exM = runStateT stM (FrontState mempty)
  ioM <- runExceptT exM
  case ioM of
    Left err -> return $ Left err
    Right (mods, fstate) -> return $ Right $ FrontendResult (fstate'modules fstate) (fmap (strip . moduleName) mods)

-- | `processSchemas modFps` traverses all .lbf schema filepaths in `modFps` and returns modules if successful.
processSchemas :: [FilePath] -> FrontendT m [Module SourceInfo]
processSchemas modFps = processSchema `traverse` modFps

-- | `processSchema modFp` takes a single .lbf schema in `modFp`, parses it, validates it, and collects its type and class scope. If the module has already been processed it returns it from the cache.
processSchema :: FilePath -> FrontendT m (Module SourceInfo)
processSchema modFp = do
  modContent <- liftIO $ Text.readFile modFp
  m <- parseModule modFp modContent
  checkModuleName modFp (moduleName m)
  ims <- gets fstate'modules
  case Map.lookup (strip $ moduleName m) ims of
    Nothing -> processModule m
    Just (m', _) -> return m'

-- | 'checkModuleName modFp modName' checks whether the module filepath in `modFp` 'a/b/c/X/Y/Z.lbf' has the module name 'X.Y.Z' suffix (eq. ''X/Y/Z.lbf')
checkModuleName :: FilePath -> ModuleName SourceInfo -> FrontendT m ()
checkModuleName fp mn =
  let suffix = moduleNameToFilepath mn
   in if suffix `isSuffixOf` fp
        then return ()
        else throwE' $ Errors.InvalidModuleFilepath mn fp suffix

-- | `processModule m` does heavy lifting as it processes imports which checks cycles and collects the scope. It also checks that all the references in the file are accounted for, whether by local or imported (foreign) class or type defs.
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
    _ <- lift $ modify (FrontState . Map.insert (strip . moduleName $ m) (m, modScope) . fstate'modules)
    return m

-- | `moduleNameToFilepath m` makes a filepath for the module in `m` (eq. 'X.Y.Z' -> 'X/Y/Z.lbf').
moduleNameToFilepath :: ModuleName info -> FilePath
moduleNameToFilepath (ModuleName parts _) = joinPath [unpack p | ModuleNamePart p _ <- parts] <.> "lbf"

-- | `checkCycle imp` checks whether the import in `imp` is a module that was already visited.
checkCycle :: Import SourceInfo -> FrontendT m ()
checkCycle imp = do
  ms <- asks visited
  cm <- asks current
  when ((strip . importModuleName $ imp) `elem` ms) $ throwE' $ Errors.ImportCycleFound cm imp ms

-- | `parseModule fp txt` parses a .lbf schema with a specified filename `fp` (for reporting) and content in `txt`.
parseModule :: FilePath -> Text -> FrontendT m (Module SourceInfo)
parseModule modFp modContent = do
  modOrErr <- liftIO $ Parsec.runParser Parsec.parseModule modFp modContent
  case modOrErr of
    Left err -> throwE' $ Errors.ModuleParseError modFp err
    Right m -> return m

-- | `importModule imp` imports a module specified by the import statement in `imp`. It it already imported the module it takes it from the 'cache'.
importModule :: Import SourceInfo -> FrontendT m (Module SourceInfo)
importModule imp = do
  let modName = importModuleName imp
  ims <- gets fstate'modules
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

-- | `processImports m` process all import statements in module `m` and return the collected imported scope of types and classes.
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

-- | `processImport imp` process an import statements in `imp` and return the collected imported scope of types and classes. Checks for the cycle!
processImport :: Import SourceInfo -> FrontendT m (TyScope, ClassScope)
processImport imp = do
  checkCycle imp
  im <- importModule imp >>= processModule
  collectImportedScope imp im
