module LambdaBuffers.Frontend.Cli.Utils (logInfo, logError, logFrontendError, logCompilerError, logCodegenError) where

import LambdaBuffers.Frontend (FrontendError)
import LambdaBuffers.Frontend.PPrint ()

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
