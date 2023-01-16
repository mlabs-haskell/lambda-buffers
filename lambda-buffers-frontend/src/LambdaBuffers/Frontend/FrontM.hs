module LambdaBuffers.Frontend.FrontM (runFrontM, FrontErr (..)) where

import Control.Monad (foldM, void, when)
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
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Parsec qualified as Parsec
import LambdaBuffers.Frontend.Syntax (Constructor (Constructor), Import (Import, importInfo, importModuleName), Module (moduleImports, moduleName, moduleTyDefs), ModuleAlias (ModuleAlias), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Product (Product), SourceInfo, Ty (TyApp, TyRef', TyVar), TyBody (Opaque, Sum), TyDef (TyDef, tyBody, tyDefInfo, tyName), TyName (TyName), TyRef (TyRef))
import Prettyprinter (Doc, LayoutOptions (layoutPageWidth), PageWidth (Unbounded), Pretty (pretty), defaultLayoutOptions, layoutPretty, (<+>))
import Prettyprinter.Render.String (renderShowS)
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
  }
  deriving stock (Eq, Show)

type Symbol = TyRef ()
type Scope = Map Symbol (ModuleName SourceInfo)

data FrontErr
  = ModuleNotFound (ModuleName SourceInfo) (Import SourceInfo) [FilePath]
  | MultipleModulesFound (ModuleName SourceInfo) (Import SourceInfo) [FilePath]
  | ImportCycleFound (ModuleName SourceInfo) (Import SourceInfo) [ModuleName ()]
  | ModuleParseError FilePath ParseError
  | ImportedNotFound (ModuleName SourceInfo) (ModuleName SourceInfo) (TyName SourceInfo) (Set (TyName SourceInfo))
  | InvalidModuleFilepath (ModuleName SourceInfo) FilePath FilePath
  | SymbolAlreadyImported (ModuleName SourceInfo) (Import SourceInfo) Symbol (ModuleName SourceInfo)
  | TyRefNotFound (ModuleName SourceInfo) (TyRef SourceInfo) Scope
  | DuplicateTyDef (ModuleName SourceInfo) (TyDef SourceInfo)
  | TyDefNameConflict (ModuleName SourceInfo) (TyDef SourceInfo) (ModuleName SourceInfo)
  deriving stock (Eq)

showOneLine :: Doc a -> String
showOneLine d = (renderShowS . layoutPretty (defaultLayoutOptions {layoutPageWidth = Unbounded}) $ d) ""

instance Show FrontErr where
  show (ModuleNotFound _cm imp impPaths) = showOneLine $ pretty (importInfo imp) <+> "Module" <+> pretty (importModuleName imp) <+> "not found in available import paths" <+> pretty impPaths
  show (MultipleModulesFound _cm imp conflictingPaths) = showOneLine $ pretty (importInfo imp) <+> "Module" <+> pretty (importModuleName imp) <+> "found in multiple files" <+> pretty conflictingPaths
  show (ImportCycleFound _cm imp visited) = showOneLine $ pretty (importInfo imp) <+> "Tried to load module" <+> pretty (importModuleName imp) <+> "which constitutes a cycle" <+> pretty visited
  show (ModuleParseError _fp err) = showOneLine $ pretty err
  show (ImportedNotFound _cm mn tn@(TyName _ info) available) = showOneLine $ pretty info <+> "Type" <+> pretty tn <+> "not found in module" <+> pretty mn <> ", did you mean one of" <+> pretty (Set.toList available)
  show (InvalidModuleFilepath mn@(ModuleName _ info) gotModFp wantedFpSuffix) = showOneLine $ pretty info <+> "File name" <+> pretty gotModFp <+> "doesn't match module name" <+> pretty mn <+> "expected" <+> pretty wantedFpSuffix
  show (SymbolAlreadyImported _cm imp sym alreadyInModuleName) = showOneLine $ pretty (importInfo imp) <+> "Symbol" <+> pretty sym <+> "already imported from module" <+> pretty alreadyInModuleName
  show (TyRefNotFound _cm tyR@(TyRef _ _ info) scope) = showOneLine $ pretty info <+> "Type " <> pretty tyR <+> "not found in the module's scope" <+> (pretty . Map.keys $ scope)
  show (DuplicateTyDef _cm tyDef) = showOneLine $ pretty (tyDefInfo tyDef) <+> "Duplicate type definition with the name" <+> pretty (tyName tyDef)
  show (TyDefNameConflict _cm tyDef imn) = showOneLine $ pretty (tyDefInfo tyDef) <+> "Type name" <+> pretty (tyName tyDef) <+> "conflicts with an imported type name from module" <+> pretty imn

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

checkCycle :: Import SourceInfo -> FrontM ()
checkCycle imp = do
  ms <- asks visited
  cm <- asks current
  when ((strip . importModuleName $ imp) `elem` ms) $ throwE' $ ImportCycleFound cm imp ms

parseModule :: FilePath -> Text -> FrontM (Module SourceInfo)
parseModule modFp modContent = do
  modOrErr <- liftIO $ Parsec.runParser Parsec.parseModule modFp modContent
  case modOrErr of
    Left err -> throwE' $ ModuleParseError modFp err
    Right m -> return m

strip :: Functor f => f a -> f ()
strip = void

importModule :: Import SourceInfo -> FrontM (Module SourceInfo)
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
          throwE' $ ModuleNotFound cm imp ips
        [modFp] -> do
          modContent <- liftIO $ Text.readFile modFp
          modOrErr <- liftIO $ Parsec.runParser Parsec.parseModule modFp modContent
          case modOrErr of
            Left err -> throwE' $ ModuleParseError modFp err
            Right m -> return m
        modFps -> do
          cm <- asks current
          throwE' $ MultipleModulesFound cm imp modFps
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
    importedScope <- processImports m
    localScope <- collectLocalScope m importedScope
    checkReferences (localScope <> importedScope) m
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
  checkCycle imp
  im <- importModule imp >>= processModule
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

collectLocalScope :: Module SourceInfo -> Scope -> FrontM Scope
collectLocalScope m importedScope =
  foldM
    ( \totalScope tyDef@(TyDef tn _ _ _) -> do
        let tyR = TyRef Nothing (strip tn) ()
        case Map.lookup tyR totalScope of
          Nothing -> case Map.lookup tyR importedScope of
            Nothing -> return $ Map.insert tyR (moduleName m) totalScope
            Just im -> do
              cm <- asks current
              throwE' $ TyDefNameConflict cm tyDef im
          Just _ -> do
            cm <- asks current
            throwE' $ DuplicateTyDef cm tyDef
    )
    Map.empty
    (moduleTyDefs m)
