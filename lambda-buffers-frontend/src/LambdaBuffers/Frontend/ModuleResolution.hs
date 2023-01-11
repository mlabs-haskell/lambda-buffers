module LambdaBuffers.Frontend.ModuleResolution (processImport, runFrontM, processFile) where

import Control.Monad (foldM, when)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Strict (MonadIO (liftIO), MonadTrans (lift), StateT (runStateT), gets, modify)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks, local)
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
import Debug.Trace (trace)
import LambdaBuffers.Frontend.Parsec qualified as Parsec
import LambdaBuffers.Frontend.Syntax (Constructor (Constructor), Import (Import, importModuleName), Module (moduleImports, moduleName, moduleTyDefs), ModuleAlias (ModuleAlias), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Product (Product), Ty (TyApp, TyRef', TyVar), TyBody (Opaque, Sum), TyDef (TyDef, tyBody), TyName (TyName), TyRef (TyRef))
import System.Directory (findFiles)
import System.FilePath (joinPath)
import Text.Parsec (ParseError)

data ImportRead = ImportRead
  { current :: ModuleName
  , visited :: [ModuleName]
  , importPaths :: [FilePath]
  }
  deriving stock (Eq, Show)

newtype ImportState = ImportState
  { importedModules :: Map ModuleName Module
  }
  deriving stock (Eq, Show)

type Symbol = (Maybe SModuleAlias, STyName)
type Scope = Map Symbol ModuleName

data ImportErr
  = ModuleNotFound ModuleName [FilePath]
  | MultipleModulesFound ModuleName [FilePath]
  | ImportCycleFound ModuleName ModuleName [ModuleName]
  | ModuleParseError FilePath ParseError
  | ImportedNotFound ModuleName ModuleName TyName (Set TyName)
  | InvalidModuleFilepath ModuleName FilePath FilePath
  | SymbolAlreadyImported ModuleName Import (Maybe SModuleAlias, STyName) ModuleName
  | TyRefNotFound ModuleName TyRef Scope
  deriving stock (Eq)

instance Show ImportErr where
  show (ModuleNotFound mn impPaths) = "Module " <> show mn <> " not found in import paths " <> show impPaths
  show (MultipleModulesFound mn conflictingPaths) = "Module " <> show mn <> " found in multiple files " <> show conflictingPaths
  show (ImportCycleFound current mn visited) = "Module " <> show current <> " tried to load module " <> show mn <> " which constitutes a cycle " <> show visited
  show (ModuleParseError fp err) = "Module in file " <> show fp <> " failed to parse with: " <> show err
  show (ImportedNotFound current mn tn available) = "Module " <> show current <> " tried to load type: " <> show tn <> " but none was found in " <> show mn <> ", did you mean one of " <> show available
  show (InvalidModuleFilepath mn gotModFp wantedFpSuffix) = "Module " <> show mn <> " loaded from a file path " <> gotModFp <> " but the file path must have the (module name derived) suffix " <> wantedFpSuffix
  show (SymbolAlreadyImported current imp sym alreadyInModuleName) = "Module " <> show current <> " imports a symbol " <> show sym <> " in the import statement " <> show imp <> " but the same symbol was already imported by " <> show alreadyInModuleName
  show (TyRefNotFound current tyR scope) = "Module " <> show current <> " references a type " <> show tyR <> " that wasn't found in the module scope " <> show scope

type ImportM = ReaderT ImportRead (StateT ImportState (ExceptT ImportErr IO))

runFrontM :: [FilePath] -> FilePath -> IO (Either ImportErr (Map ModuleName Module))
runFrontM importPaths modFp = do
  let stM = runReaderT (processFile modFp) (ImportRead (ModuleName [] undefined) [] importPaths)
      exM = runStateT stM (ImportState Map.empty)
      ioM = runExceptT exM
  fmap (importedModules . snd) <$> ioM

moduleNameToFilepath :: ModuleName -> FilePath
moduleNameToFilepath (ModuleName parts _) = joinPath [unpack p | ModuleNamePart p _ <- parts] <> ".lbf"

throwE' :: forall {a :: Type}. ImportErr -> ImportM a
throwE' = lift . lift . throwE

checkCycle :: ModuleName -> ImportM ()
checkCycle modName = do
  ms <- asks visited
  cm <- asks current
  when (show modName `elem` (show <$> ms)) $ throwE' $ ImportCycleFound cm modName ms

parseModule :: FilePath -> Text -> ImportM Module
parseModule modFp modContent = do
  modOrErr <- liftIO $ Parsec.runParser Parsec.parseModule modFp modContent
  case modOrErr of
    Left err -> throwE' $ ModuleParseError modFp err
    Right m -> return m

readModule :: ModuleName -> ImportM Module
readModule modName = do
  ims <- gets importedModules
  case Map.lookup modName ims of
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

processFile :: FilePath -> ImportM Module
processFile modFp = do
  modContent <- liftIO $ Text.readFile modFp
  m <- parseModule modFp modContent
  checkModuleName modFp (moduleName m)
  processModule m

processModule :: Module -> ImportM Module
processModule m = local (\ir -> ir {current = moduleName m, visited = current ir : visited ir}) $ do
  r <- ask
  liftIO $ trace (show r) (return ())
  scope <- processImports m
  checkReferences scope m
  _ <- lift $ modify (ImportState . Map.insert (moduleName m) m . importedModules)
  return m

checkReferences :: Map (Maybe SModuleAlias, STyName) ModuleName -> Module -> ImportM ()
checkReferences scope m = for_ (moduleTyDefs m) (checkBody . tyBody)
  where
    checkBody (Sum cs _) = for_ cs checkConstructor
    checkBody Opaque = return ()

    checkConstructor (Constructor _ (Product tys _) _) = for tys checkTy

    checkTy (TyApp tyF tyAs _) = checkTy tyF >> for_ tyAs checkTy
    checkTy (TyVar _ _) = return ()
    checkTy (TyRef' tyR@(TyRef mayModAlias tyN _) _) =
      if Map.member (fromModuleAlias <$> mayModAlias, fromTyName tyN) scope
        then return ()
        else do
          cm <- asks current
          throwE' $ TyRefNotFound cm tyR scope

checkModuleName :: FilePath -> ModuleName -> ImportM ()
checkModuleName fp mn =
  let suffix = moduleNameToFilepath mn
   in if suffix `isSuffixOf` fp
        then return ()
        else throwE' $ InvalidModuleFilepath mn fp suffix

processImports :: Module -> ImportM (Map (Maybe SModuleAlias, STyName) ModuleName)
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

processImport :: Import -> ImportM (Set (Maybe SModuleAlias, STyName))
processImport imp = do
  liftIO $ trace (show imp) (return ())
  let modNameToImport = importModuleName imp
  checkCycle modNameToImport
  im <- readModule modNameToImport >>= processModule
  collectImportedScope imp im

newtype STyName = STyName Text deriving stock (Eq, Ord, Show)
fromTyName :: TyName -> STyName
fromTyName (TyName tn _) = STyName tn

newtype SModuleAlias = SModuleAlias [Text] deriving stock (Eq, Ord, Show)

fromModuleAlias :: ModuleAlias -> SModuleAlias
fromModuleAlias (ModuleAlias mn _) = SModuleAlias . fromModuleName $ mn

fromModuleName :: ModuleName -> [Text]
fromModuleName (ModuleName ps _) = [p | ModuleNamePart p _ <- ps]

collectImportedScope :: Import -> Module -> ImportM (Set (Maybe SModuleAlias, STyName))
collectImportedScope (Import isQual modName mayImports mayAlias _) m =
  let availableTyNs = [tyN | (TyDef tyN _ _ _) <- moduleTyDefs m]
      availableTyNs' = Set.fromList availableTyNs
      importedTyNs = fromMaybe availableTyNs mayImports
      availableSTyNs = Set.fromList $ fromTyName <$> availableTyNs
   in foldM
        ( \total tyN ->
            let sTyN = fromTyName tyN
             in if Set.member sTyN availableSTyNs
                  then return . Set.union total . Set.fromList $
                    case mayAlias of
                      Nothing ->
                        if isQual
                          then [(Just . SModuleAlias . fromModuleName $ modName, sTyN)]
                          else [(Just . SModuleAlias . fromModuleName $ modName, sTyN), (Nothing, sTyN)]
                      Just al ->
                        if isQual
                          then [(Just . fromModuleAlias $ al, sTyN)]
                          else [(Just . fromModuleAlias $ al, sTyN), (Nothing, sTyN)]
                  else do
                    currModName <- asks current
                    throwE' $ ImportedNotFound currModName modName tyN availableTyNs'
        )
        Set.empty
        importedTyNs
