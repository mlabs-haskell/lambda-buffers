module LambdaBuffers.Frontend.Cli.Utils (logInfo, logError, logFrontendError, logCompilerError, logCodegenError, toCodegenCliModuleName) where

import Data.List (intercalate)
import Data.Text qualified as Text
import LambdaBuffers.Frontend (FrontendError)
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Syntax qualified as Frontend

logInfo :: String -> IO ()
logInfo msg = putStrLn $ "[lbf][INFO] " <> msg

logError :: String -> IO ()
logError msg = putStrLn $ "[lbf][ERROR] " <> msg

logFrontendError :: FrontendError -> IO ()
logFrontendError err = putStrLn $ "[lbf][ERROR]" <> show err

logCompilerError :: String -> IO ()
logCompilerError msg = putStrLn $ "[lbf][ERROR][COMPILER]" <> msg

logCodegenError :: String -> IO ()
logCodegenError msg = putStrLn $ "[lbf][ERROR][CODEGEN]" <> msg

-- NOTE(bladyjoker): Consider using the proto to supply requested modules.
toCodegenCliModuleName :: Frontend.ModuleName () -> String
toCodegenCliModuleName (Frontend.ModuleName parts _) = intercalate "." ((\(Frontend.ModuleNamePart p _) -> Text.unpack p) <$> parts)
