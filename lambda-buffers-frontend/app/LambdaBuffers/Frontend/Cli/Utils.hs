module LambdaBuffers.Frontend.Cli.Utils (logInfo, logError, logFrontendError, logCompilerError, logCodegenError, toCodegenCliModuleName, checkExists, FileOrDir (..)) where

import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Text qualified as Text
import LambdaBuffers.Frontend (FrontendError)
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Syntax qualified as Frontend
import System.Directory (doesPathExist)
import System.Exit (exitFailure)

logInfo :: FilePath -> String -> IO ()
logInfo "" msg = putStrLn $ msg <> " [INFO]"
logInfo fp msg = for_ (lines msg) $ \l -> putStrLn $ fp <> ": " <> l <> " [INFO]"

logError :: FilePath -> String -> IO ()
logError "" msg = putStrLn $ msg <> " [ERROR]"
logError fp msg = for_ (lines msg) $ \l -> putStrLn $ fp <> ": " <> l <> " [ERROR]"

logFrontendError :: FrontendError -> IO ()
logFrontendError err = putStrLn $ show err <> " [ERROR]"

logCompilerError :: FilePath -> String -> IO ()
logCompilerError lbcFp msg = putStrLn $ lbcFp <> ":" <> msg <> " [ERROR]"

logCodegenError :: FilePath -> String -> IO ()
logCodegenError lbgFp msg = putStrLn $ lbgFp <> ":" <> msg <> " [ERROR]"

-- NOTE(bladyjoker): Consider using the proto to supply requested modules.
toCodegenCliModuleName :: Frontend.ModuleName () -> String
toCodegenCliModuleName (Frontend.ModuleName parts _) = intercalate "." ((\(Frontend.ModuleNamePart p _) -> Text.unpack p) <$> parts)

data FileOrDir = File | Dir deriving stock (Eq)

checkExists :: FileOrDir -> String -> FilePath -> IO ()
checkExists fileOrDir purpose path = do
  exists <- doesPathExist path
  if exists
    then return ()
    else do
      logError "" $
        "The provided "
          <> purpose
          <> " "
          <> ( if fileOrDir == File
                then "file"
                else "directory"
             )
          <> " "
          <> path
          <> " doesn't exist"
      exitFailure
