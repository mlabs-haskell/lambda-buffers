module LambdaBuffers.Frontend.Cli.Build (BuildOpts (..), build) where

import Control.Lens (makeLenses, (^.))
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.ProtoLens (Message (defMessage))
import Data.ProtoLens.Encoding qualified as Pb
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.IO qualified as LText
import Data.Text.Lazy.IO qualified as Text
import LambdaBuffers.Frontend (FrontendError, runFrontend)
import LambdaBuffers.Frontend.Errors.Compiler qualified as CompilerErrors
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.ToProto (toCompilerInput)
import Proto.Compiler qualified as Compiler
import Proto.Compiler_Fields qualified as Compiler
import System.Exit (exitFailure)
import System.FilePath ((<.>), (</>))
import System.FilePath.Lens (extension)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, showCommandForUser)

data BuildOpts = BuildOpts
  { _importPaths :: [FilePath]
  , _moduleFilepath :: FilePath
  , _compilerCliFilepath :: FilePath
  , _debug :: Bool
  , _workingDir :: Maybe FilePath
  }
  deriving stock (Eq, Show)

makeLenses ''BuildOpts

logInfo :: String -> IO ()
logInfo msg = putStrLn $ "[lbf][INFO] " <> msg

logError :: String -> IO ()
logError msg = putStrLn $ "[lbf][ERROR] " <> msg

logFrontendError :: FrontendError -> IO ()
logFrontendError err = putStrLn $ "[lbf][ERROR]" <> show err

logCompilerError :: String -> IO ()
logCompilerError msg = putStrLn $ "[lbf][ERROR][compiler]" <> msg

-- | Build a filepath containing a LambdaBuffers module
build :: BuildOpts -> IO ()
build opts = do
  errOrMod <- runFrontend (opts ^. importPaths) (opts ^. moduleFilepath)
  case errOrMod of
    Left err -> logFrontendError err
    Right res -> do
      logInfo "OK"
      compInp <- either (\e -> error $ "Failed translating to Compiler proto: " <> show e) return (toCompilerInput res)
      withSystemTempDirectory
        "lambda-buffers-frontend"
        ( \tempDir -> do
            _compRes <- callCompiler opts tempDir compInp
            logInfo "Compilation OK"
            return ()
        )

callCompiler :: BuildOpts -> FilePath -> Compiler.Input -> IO Compiler.Result
callCompiler opts tempDir compInp = do
  let ext = if opts ^. debug then "textproto" else "pb"
      workDir = fromMaybe tempDir (opts ^. workingDir)
      compInpFp = workDir </> "compiler-input" <.> ext
      compOutFp = workDir </> "compiler-output" <.> ext
  writeCompilerInput compInpFp compInp
  let args = ["compile", "--input-file", compInpFp, "--output-file", compOutFp]
  logInfo $ "Calling: " <> showCommandForUser (opts ^. compilerCliFilepath) args
  callProcess (opts ^. compilerCliFilepath) args
  compOut <- readCompilerOutput compOutFp
  if compOut ^. Compiler.error == defMessage
    then return $ compOut ^. Compiler.result
    else do
      let serrs = CompilerErrors.showErrors (compOut ^. Compiler.error)
      for_ serrs logCompilerError
      exitFailure

writeCompilerInput :: FilePath -> Compiler.Input -> IO ()
writeCompilerInput fp compInp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> BS.writeFile fp (Pb.encodeMessage compInp)
    ".textproto" -> Text.writeFile fp (Text.pack . show $ PbText.pprintMessage compInp)
    _ -> do
      logError $ "Unknown Compiler Input format (wanted .pb or .textproto) " <> ext
      exitFailure

readCompilerOutput :: FilePath -> IO Compiler.Output
readCompilerOutput fp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> do
      content <- BS.readFile fp
      case Pb.decodeMessage content of
        Left err -> do
          logError $ "Failed decoding the Compiler Output\n" <> err
          exitFailure
        Right res -> return res
    ".textproto" -> do
      content <- LText.readFile fp
      return $ PbText.readMessageOrDie content
    _ -> do
      logError $ "Unknown Compiler Output format (wanted .pb or .textproto) " <> ext
      exitFailure
