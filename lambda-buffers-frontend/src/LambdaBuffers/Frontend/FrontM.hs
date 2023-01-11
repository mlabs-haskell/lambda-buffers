module LambdaBuffers.Frontend.FrontM (runFrontM) where

import Control.Monad (foldM, when)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Strict (MonadIO (liftIO), MonadTrans (lift), StateT (runStateT), gets, modify)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, local)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.List (isSuffixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Data.Text.IO qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Frontend.Parsec qualified as Parsec
import LambdaBuffers.Frontend.Syntax (Constructor (Constructor), Import (Import, importModuleName), Module (moduleImports, moduleName, moduleTyDefs), ModuleAlias (ModuleAlias), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Product (Product), SourceInfo, Strip (strip), Ty (TyApp, TyRef', TyVar), TyBody (Opaque, Sum), TyDef (TyDef, tyBody), TyName, TyRef (TyRef))
import System.Directory (findFiles)
import System.FilePath (joinPath)
import Text.Parsec (ParseError)

data FrontRead = FrontRead
  { current :: ModuleName SourceInfo
  , visited :: [ModuleName ()]
  , importPaths :: [FilePath]
  }
  deriving stock (Eq, Show)

newtype FrontState = FrontState
  { importedModules :: Map (ModuleName ()) (Module SourceInfo)
  } -- deriving stock (Eq, Show)

type Symbol = TyRef ()
type Scope = Map Symbol (ModuleName SourceInfo)

data FrontErr
  = ModuleNotFound (ModuleName SourceInfo) [FilePath]
  | MultipleModulesFound (ModuleName SourceInfo) [FilePath]
  | ImportCycleFound (ModuleName SourceInfo) (ModuleName ()) [ModuleName ()]
  | ModuleParseError FilePath ParseError
  | ImportedNotFound (ModuleName SourceInfo) (ModuleName SourceInfo) (TyName SourceInfo) (Set (TyName SourceInfo))
  | InvalidModuleFilepath (ModuleName SourceInfo) FilePath FilePath
  | SymbolAlreadyImported (ModuleName SourceInfo) (Import SourceInfo) Symbol (ModuleName SourceInfo)
  | TyRefNotFound (ModuleName SourceInfo) (TyRef SourceInfo) Scope
  deriving stock (Eq)

instance Show FrontErr where
  show (ModuleNotFound mn impPaths) = "Module " <> show mn <> " not found in import paths " <> show impPaths
  show (MultipleModulesFound mn conflictingPaths) = "Module " <> show mn <> " found in multiple files " <> show conflictingPaths
  show (ImportCycleFound current mn visited) = "Module " <> show current <> " tried to load module " <> show mn <> " which constitutes a cycle " <> show visited
  show (ModuleParseError fp err) = "Module in file " <> show fp <> " failed to parse with: " <> show err
  show (ImportedNotFound current mn tn available) = "Module " <> show current <> " tried to load type: " <> show tn <> " but none was found in " <> show mn <> ", did you mean one of " <> show available
  show (InvalidModuleFilepath mn gotModFp wantedFpSuffix) = "Module " <> show mn <> " loaded from a file path " <> gotModFp <> " but the file path must have the (module name derived) suffix " <> wantedFpSuffix
  show (SymbolAlreadyImported current imp sym alreadyInModuleName) = "Module " <> show current <> " imports a symbol " <> show sym <> " in the import statement " <> show imp <> " but the same symbol was already imported by " <> show alreadyInModuleName
  show (TyRefNotFound current tyR scope) = "Module " <> show current <> " references a type " <> show tyR <> " that wasn't found in the module scope " <> show scope

type FrontM = ReaderT FrontRead (StateT FrontState (ExceptT FrontErr IO))

runFrontM :: [FilePath] -> FilePath -> IO (Either FrontErr (Map (ModuleName ()) (Module SourceInfo)))
runFrontM importPaths modFp = do
  let stM = runReaderT (processFile modFp) (FrontRead (ModuleName [] undefined) [] importPaths)
      exM = runStateT stM (FrontState Map.empty)
      ioM = runExceptT exM
  fmap (importedModules . snd) <$> ioM

throwE' :: forall {a :: Type}. FrontErr -> FrontM a
throwE' = lift . lift . throwE

moduleNameToFilepath :: ModuleName info -> FilePath
moduleNameToFilepath (ModuleName parts _) = joinPath [unpack p | ModuleNamePart p _ <- parts] <> ".lbf"

checkCycle :: ModuleName () -> FrontM ()
checkCycle modName = do
  ms <- asks visited
  cm <- asks current
  when (modName `elem` ms) $ throwE' $ ImportCycleFound cm modName ms

parseModule :: FilePath -> Text -> FrontM (Module SourceInfo)
parseModule modFp modContent = do
  modOrErr <- liftIO $ Parsec.runParser Parsec.parseModule modFp modContent
  case modOrErr of
    Left err -> throwE' $ ModuleParseError modFp err
    Right m -> return m

readModule :: ModuleName SourceInfo -> FrontM (Module SourceInfo)
readModule modName = do
  ims <- gets importedModules
  case Map.lookup (strip modName) ims of
    Nothing -> do
      ips <- asks importPaths
      found <- liftIO $ findFiles ips (moduleNameToFilepath modName)
      case found of
        [] -> throwE' $ ModuleNotFound modName ips
        [modFp] -> do
          modContent <- liftIO $ Text.readFile modFp
          modOrErr <- liftIO $ Parsec.runParser Parsec.parseModule modFp modContent
          case modOrErr of
            Left err -> throwE' $ ModuleParseError modFp err
            Right m -> return m
        modFps -> throwE' $ MultipleModulesFound modName modFps
    Just m -> return m

processFile :: FilePath -> FrontM (Module SourceInfo)
processFile modFp = do
  modContent <- liftIO $ Text.readFile modFp
  m <- parseModule modFp modContent
  checkModuleName modFp (moduleName m)
  processModule m

processModule :: Module SourceInfo -> FrontM (Module SourceInfo)
processModule m = local
  ( \ir ->
      ir
        { current = moduleName m
        , visited = (strip . current $ ir) : visited ir
        }
  )
  $ do
    scope <- processImports m
    checkReferences scope m
    _ <- lift $ modify (FrontState . Map.insert (strip . moduleName $ m) m . importedModules)
    return m

checkReferences :: Scope -> Module SourceInfo -> FrontM ()
checkReferences scope m = for_ (moduleTyDefs m) (checkBody . tyBody)
  where
    checkBody (Sum cs _) = for_ cs checkConstructor
    checkBody Opaque = return ()

    checkConstructor (Constructor _ (Product tys _) _) = for tys checkTy

    checkTy (TyApp tyF tyAs _) = checkTy tyF >> for_ tyAs checkTy
    checkTy (TyVar _ _) = return ()
    checkTy (TyRef' tyR _) =
      if Map.member (strip tyR) scope
        then return ()
        else do
          cm <- asks current
          throwE' $ TyRefNotFound cm tyR scope

checkModuleName :: FilePath -> ModuleName SourceInfo -> FrontM ()
checkModuleName fp mn =
  let suffix = moduleNameToFilepath mn
   in if suffix `isSuffixOf` fp
        then return ()
        else throwE' $ InvalidModuleFilepath mn fp suffix

processImports :: Module SourceInfo -> FrontM Scope
processImports m =
  foldM
    ( \totalScope imp -> do
        scope <- processImport imp
        foldM
          ( \totalScope' sym -> case Map.lookup sym totalScope' of
              Nothing -> return $ Map.insert sym (importModuleName imp) totalScope'
              Just mn -> do
                cm <- asks current
                throwE' $ SymbolAlreadyImported cm imp sym mn
          )
          totalScope
          scope
    )
    Map.empty
    (moduleImports m)

processImport :: Import SourceInfo -> FrontM (Set Symbol)
processImport imp = do
  let modNameToImport = importModuleName imp
  checkCycle (strip modNameToImport)
  im <- readModule modNameToImport >>= processModule
  collectImportedScope imp im

collectImportedScope :: Import SourceInfo -> Module SourceInfo -> FrontM (Set Symbol)
collectImportedScope (Import isQual modName mayImports mayAlias _) m =
  let availableTyNs = [tyN | (TyDef tyN _ _ _) <- moduleTyDefs m]
      availableTyNs' = Set.fromList availableTyNs
      importedTyNs = fromMaybe availableTyNs mayImports
      availableSTyNs = Set.fromList $ strip <$> availableTyNs
   in foldM
        ( \total tyN ->
            let styN = strip tyN
             in if Set.member styN availableSTyNs
                  then return . Set.union total . Set.fromList $
                    case mayAlias of
                      Nothing ->
                        if isQual
                          then [TyRef (Just $ ModuleAlias (strip modName) ()) styN ()]
                          else [TyRef (Just $ ModuleAlias (strip modName) ()) styN (), TyRef Nothing styN ()]
                      Just al ->
                        if isQual
                          then [TyRef (Just . strip $ al) styN ()]
                          else [TyRef (Just . strip $ al) styN (), TyRef Nothing styN ()]
                  else do
                    cm <- asks current
                    throwE' $ ImportedNotFound cm modName tyN availableTyNs'
        )
        Set.empty
        importedTyNs
