module LambdaBuffers.Frontend.ModuleResolution (importModule, runImportM) where

import Control.Monad (when)
import Control.Monad.State.Strict (MonadIO (liftIO), MonadTrans (lift), StateT (runStateT), modify)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, local)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (unpack)
import LambdaBuffers.Frontend.Parsec (parseTest')
import LambdaBuffers.Frontend.Parsec qualified as Parsec
import LambdaBuffers.Frontend.Syntax (Import (Import), Module (moduleImports), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart))
import System.Directory (findFiles)
import System.FilePath (joinPath)

data ImportRead = ImportRead
  { visited :: [ModuleName]
  , importPaths :: [FilePath]
  }
  deriving stock (Eq, Show)

newtype ImportState = ImportState
  { importedModules :: Map ModuleName Module
  }
  deriving stock (Eq, Show)

type ImportM = ReaderT ImportRead (StateT ImportState IO)

moduleNameToFilepath :: ModuleName -> FilePath
moduleNameToFilepath (ModuleName parts _) = joinPath [unpack p | ModuleNamePart p _ <- parts]

readModule :: ModuleName -> ImportM String
readModule modName = do
  ips <- asks importPaths
  let modBaseName = moduleNameToFilepath modName
  found <- liftIO $ findFiles ips (modBaseName <> ".lbf")
  case found of
    [] -> error $ "Can't find module " <> modBaseName <> " in import paths " <> show ips
    [moduleFp] -> liftIO $ readFile moduleFp
    _ -> error $ "Found multiple module " <> modBaseName <> " in import paths"

checkCycle :: ModuleName -> ImportM ()
checkCycle modName = do
  ms <- asks visited
  when (modName `elem` ms) $ error $ "There's an import cycle when importing module " <> show modName <> " after visiting " <> show ms

parseModule :: String -> ImportM Module
parseModule modContent = do
  modOrErr <- liftIO $ parseTest' Parsec.parseModule modContent
  case modOrErr of
    Left err -> error $ "Failed parsing module " <> err
    Right m -> return m

importModule :: Import -> ImportM Module
importModule (Import isQual modName imports mayAlias _) = do
  checkCycle modName
  modContent <- readModule modName
  m <- parseModule modContent
  for_
    (moduleImports m)
    ( \im -> do
        local (\(ImportRead vs ips) -> ImportRead (modName : vs) ips) (importModule im)
    )
  _ <- lift $ modify (ImportState . Map.insert modName m . importedModules)
  return m

runImportM :: [FilePath] -> ModuleName -> Import -> IO (Map ModuleName Module)
runImportM importPaths modName imp = do
  let stM = runReaderT (importModule imp) (ImportRead [modName] importPaths)
      ioM = runStateT stM (ImportState Map.empty)
  importedModules . snd <$> ioM
