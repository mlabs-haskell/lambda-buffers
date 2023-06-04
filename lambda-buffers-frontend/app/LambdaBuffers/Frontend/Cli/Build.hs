module LambdaBuffers.Frontend.Cli.Build (BuildOpts (..), build) where

import Control.Lens (makeLenses, (&), (.~), (^.))
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.ProtoLens (Message (defMessage))
import Data.ProtoLens.Encoding qualified as Pb
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.IO qualified as LText
import Data.Text.Lazy.IO qualified as Text
import LambdaBuffers.Frontend (runFrontend)
import LambdaBuffers.Frontend.Cli.Env qualified as Env
import LambdaBuffers.Frontend.Cli.Utils (logCodegenError, logCompilerError, logError, logFrontendError, logInfo)
import LambdaBuffers.Frontend.Errors.Codegen qualified as CodegenErrors
import LambdaBuffers.Frontend.Errors.Compiler qualified as CompilerErrors
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.ToProto (toModules)
import Proto.Codegen qualified as Codegen
import Proto.Codegen_Fields qualified as Codegen
import Proto.Compiler qualified as Compiler
import Proto.Compiler_Fields qualified as Compiler
import System.Exit (ExitCode (ExitFailure), exitFailure)
import System.FilePath ((<.>), (</>))
import System.FilePath.Lens (extension)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode, showCommandForUser)

data BuildOpts = BuildOpts
  { _importPaths :: [FilePath]
  , _moduleFilepath :: FilePath
  , _compilerCliFilepath :: Maybe FilePath
  , _codegenCliFilepath :: Maybe FilePath
  , _codegenGenDir :: FilePath
  , _codegenCliOpts :: [String]
  , _debug :: Bool
  , _workingDir :: Maybe FilePath
  }
  deriving stock (Eq, Show)

makeLenses ''BuildOpts

-- | Build a filepath containing a LambdaBuffers module
build :: BuildOpts -> IO ()
build opts = do
  errOrMod <- runFrontend (opts ^. importPaths) (opts ^. moduleFilepath)
  case errOrMod of
    Left err -> logFrontendError err
    Right res -> do
      logInfo "OK"
      mods <- either (\e -> error $ "Failed building Proto API modules: " <> show e) return (toModules res)
      withSystemTempDirectory
        "lambda-buffers-frontend"
        ( \tempDir -> do
            _compRes <- callCompiler opts tempDir (defMessage & Compiler.modules .~ mods)
            logInfo "Compilation OK"
            _cdgRes <- callCodegen opts tempDir (defMessage & Codegen.modules .~ mods)
            logInfo "Codegen OK"
        )

callCompiler :: BuildOpts -> FilePath -> Compiler.Input -> IO Compiler.Result
callCompiler opts tempDir compInp = do
  let ext = if opts ^. debug then "textproto" else "pb"
      workDir = fromMaybe tempDir (opts ^. workingDir)
      compInpFp = workDir </> "compiler-input" <.> ext
      compOutFp = workDir </> "compiler-output" <.> ext
  writeCompilerInput compInpFp compInp
  lbcFp <- maybe Env.getLbcFromEnvironment return (opts ^. compilerCliFilepath)
  let args = ["compile", "--input-file", compInpFp, "--output-file", compOutFp]
  call lbcFp args
  _ <- readProcessWithExitCode lbcFp args ""
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

call :: FilePath -> [String] -> IO ()
call cliFp cliArgs = do
  logInfo $ "Calling: " <> showCommandForUser cliFp cliArgs
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode cliFp cliArgs ""
  case exitCode of
    (ExitFailure _) -> do
      logError $ "Error from:" <> showCommandForUser cliFp cliArgs
      logError stdErr
      logError stdOut
      exitFailure
    _ -> do
      logInfo $ "Success from: " <> showCommandForUser cliFp cliArgs
      return ()

callCodegen :: BuildOpts -> FilePath -> Codegen.Input -> IO Codegen.Result
callCodegen opts tempDir compInp = do
  let ext = if opts ^. debug then "textproto" else "pb"
      workDir = fromMaybe tempDir (opts ^. workingDir)
      compInpFp = workDir </> "codegen-input" <.> ext
      compOutFp = workDir </> "codegen-output" <.> ext
  writeCodegenInput compInpFp compInp
  lbgFp <- maybe Env.getLbgFromEnvironment return (opts ^. codegenCliFilepath)
  let args = ["--input", compInpFp, "--output", compOutFp, "--gen-dir", opts ^. codegenGenDir] ++ opts ^. codegenCliOpts
  call lbgFp args
  compOut <- readCodegenOutput compOutFp
  if compOut ^. Codegen.error == defMessage
    then return $ compOut ^. Codegen.result
    else do
      let serrs = CodegenErrors.showErrors (compOut ^. Codegen.error)
      for_ serrs logCodegenError
      exitFailure

writeCodegenInput :: FilePath -> Codegen.Input -> IO ()
writeCodegenInput fp compInp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> BS.writeFile fp (Pb.encodeMessage compInp)
    ".textproto" -> Text.writeFile fp (Text.pack . show $ PbText.pprintMessage compInp)
    _ -> do
      logError $ "Unknown Codegen Input format (wanted .pb or .textproto) " <> ext
      exitFailure

readCodegenOutput :: FilePath -> IO Codegen.Output
readCodegenOutput fp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> do
      content <- BS.readFile fp
      case Pb.decodeMessage content of
        Left err -> do
          logError $ "Failed decoding the Codegen Output\n" <> err
          exitFailure
        Right res -> return res
    ".textproto" -> do
      content <- LText.readFile fp
      return $ PbText.readMessageOrDie content
    _ -> do
      logError $ "Unknown Codegen Output format (wanted .pb or .textproto) " <> ext
      exitFailure
