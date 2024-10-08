module LambdaBuffers.Frontend.Cli.Build (BuildOpts (..), build) where

import Control.Lens (makeLenses, (&), (.~), (^.))
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.Foldable (Foldable (toList), for_, traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.ProtoLens (Message (defMessage))
import Data.ProtoLens.Encoding qualified as Pb
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.IO qualified as LText
import Data.Text.Lazy.IO qualified as Text
import LambdaBuffers.Frontend (runFrontend)
import LambdaBuffers.Frontend.Cli.Env qualified as Env
import LambdaBuffers.Frontend.Cli.Utils (FileOrDir (Dir, File), checkExists, logCodegenError, logCompilerError, logFrontendError, toCodegenCliModuleName)
import LambdaBuffers.Frontend.Errors.Codegen qualified as CodegenErrors
import LambdaBuffers.Frontend.Errors.Compiler qualified as CompilerErrors
import LambdaBuffers.Frontend.Monad qualified as Frontend
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Syntax qualified as Frontend
import LambdaBuffers.Frontend.ToProto (toModules)
import LambdaBuffers.Utils.Logger (logError, logInfo)
import Proto.Codegen qualified as Codegen
import Proto.Codegen_Fields qualified as Codegen
import Proto.Compiler qualified as Compiler
import Proto.Compiler_Fields qualified as Compiler
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (ExitFailure), exitFailure)
import System.FilePath ((<.>), (</>))
import System.FilePath.Lens (extension)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode, showCommandForUser)

data BuildOpts = BuildOpts
  { _importPaths :: [FilePath]
  , _compilerCliFilepath :: Maybe FilePath
  , _codegenCliFilepath :: Maybe FilePath
  , _codegenGenDir :: FilePath
  , _codegenClasses :: [String]
  , _codegenCliOpts :: [String]
  , _debug :: Bool
  , _workingDir :: Maybe FilePath
  , _moduleFilepaths :: NonEmpty FilePath
  }
  deriving stock (Eq, Show)

makeLenses ''BuildOpts

-- | Build a filepath containing a LambdaBuffers module
build :: BuildOpts -> IO ()
build opts = do
  checkExists Dir "import-path" `traverse_` (opts ^. importPaths)
  checkExists File "compiler" `traverse_` (opts ^. compilerCliFilepath)
  checkExists File "gen" `traverse_` (opts ^. codegenCliFilepath)
  checkExists Dir "gen-dir" (opts ^. codegenGenDir)
  checkExists Dir "work-dir" `traverse_` (opts ^. workingDir)
  checkExists File "module" `traverse_` (opts ^. moduleFilepaths)
  errOrMod <- runFrontend ("." : opts ^. importPaths) (toList $ opts ^. moduleFilepaths)
  case errOrMod of
    Left err -> do
      logFrontendError err
      exitFailure
    Right res -> do
      mods <-
        either
          ( \e -> do
              logError "" $ "failed building Proto API modules: " <> show e
              exitFailure
          )
          return
          (toModules res)
      withSystemTempDirectory
        "lbf"
        ( \tempDir -> do
            workDir <- getWorkDir opts tempDir
            _compRes <- callCompiler opts workDir (defMessage & Compiler.modules .~ mods)
            logInfo "" "compilation OK"
            _cdgRes <- callCodegen opts workDir (Frontend.fres'requested res) (defMessage & Codegen.modules .~ mods)
            logInfo "" "codegen OK"
        )

getWorkDir :: BuildOpts -> FilePath -> IO FilePath
getWorkDir opts tempDir = do
  let workDir = fromMaybe tempDir (opts ^. workingDir)
  exists <- doesDirectoryExist workDir
  unless
    exists
    ( do
        logError workDir $ "provided working directory " <> workDir <> " doesn't exist (did you forget to create it first?)"
        exitFailure
    )
  return workDir

callCompiler :: BuildOpts -> FilePath -> Compiler.Input -> IO Compiler.Result
callCompiler opts workDir compInp = do
  let ext = if opts ^. debug then "textproto" else "pb"
      compInpFp = workDir </> "compiler-input" <.> ext
      compOutFp = workDir </> "compiler-output" <.> ext
  writeCompilerInput compInpFp compInp
  lbcFp <- maybe Env.getLbcFromEnvironment return (opts ^. compilerCliFilepath)
  let args =
        [ "compile"
        , "--input-file"
        , compInpFp
        , "--output-file"
        , compOutFp
        ]
  call (opts ^. debug) lbcFp args
  compOut <- readCompilerOutput compOutFp
  if compOut ^. Compiler.error == defMessage
    then return $ compOut ^. Compiler.result
    else do
      let serrs = CompilerErrors.showErrors (compOut ^. Compiler.error)
      for_ serrs (logCompilerError lbcFp)
      exitFailure

writeCompilerInput :: FilePath -> Compiler.Input -> IO ()
writeCompilerInput fp compInp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> BS.writeFile fp (Pb.encodeMessage compInp)
    ".textproto" -> Text.writeFile fp (Text.pack . show $ PbText.pprintMessage compInp)
    _ -> do
      logError fp $ "unknown Compiler Input format (wanted .pb or .textproto) " <> ext
      exitFailure

readCompilerOutput :: FilePath -> IO Compiler.Output
readCompilerOutput fp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> do
      content <- BS.readFile fp
      case Pb.decodeMessage content of
        Left err -> do
          logError fp $ "failed decoding the Compiler Output\n" <> err
          exitFailure
        Right res -> return res
    ".textproto" -> do
      content <- LText.readFile fp
      return $ PbText.readMessageOrDie content
    _ -> do
      logError fp $ "unknown Compiler Output format (wanted .pb or .textproto) " <> ext
      exitFailure

call :: Bool -> FilePath -> [String] -> IO ()
call dbg cliFp cliArgs = do
  when dbg $ logInfo cliFp $ "calling: " <> showCommandForUser cliFp cliArgs
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode cliFp cliArgs ""
  case exitCode of
    (ExitFailure _) -> do
      logError cliFp stdErr
      logError cliFp stdOut
      logError cliFp $ "error from:" <> showCommandForUser cliFp cliArgs
      exitFailure
    _ -> do
      when (dbg && stdOut /= "") $ logInfo cliFp stdOut
      when (dbg && stdErr /= "") $ logInfo cliFp stdErr
      logInfo cliFp $ "success from: " <> showCommandForUser cliFp cliArgs
      return ()

callCodegen :: BuildOpts -> FilePath -> [Frontend.ModuleName ()] -> Codegen.Input -> IO Codegen.Result
callCodegen opts workDir requestedModules compInp = do
  let ext = if opts ^. debug then "textproto" else "pb"
      compInpFp = workDir </> "codegen-input" <.> ext
      compOutFp = workDir </> "codegen-output" <.> ext
  writeCodegenInput compInpFp compInp
  lbgFp <- maybe Env.getLbgFromEnvironment return (opts ^. codegenCliFilepath)
  let args =
        [ "--input"
        , compInpFp
        , "--output"
        , compOutFp
        , "--gen-dir"
        , opts ^. codegenGenDir
        ]
          <> ["--debug" | opts ^. debug]
          <> mconcat [["--gen-class", cl] | cl <- opts ^. codegenClasses]
          <> (opts ^. codegenCliOpts)
          <> (toCodegenCliModuleName <$> requestedModules) -- NOTE(bladyjoker): Consider using the proto to supply requested modules.
  call (opts ^. debug) lbgFp args
  compOut <- readCodegenOutput compOutFp
  if compOut ^. Codegen.error == defMessage
    then return $ compOut ^. Codegen.result
    else do
      let serrs = CodegenErrors.showErrors (compOut ^. Codegen.error)
      for_ serrs (logCodegenError lbgFp)
      exitFailure

writeCodegenInput :: FilePath -> Codegen.Input -> IO ()
writeCodegenInput fp compInp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> BS.writeFile fp (Pb.encodeMessage compInp)
    ".textproto" -> Text.writeFile fp (Text.pack . show $ PbText.pprintMessage compInp)
    _ -> do
      logError fp $ "Unknown Codegen Input format (wanted .pb or .textproto) " <> ext
      exitFailure

readCodegenOutput :: FilePath -> IO Codegen.Output
readCodegenOutput fp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> do
      content <- BS.readFile fp
      case Pb.decodeMessage content of
        Left err -> do
          logError fp $ "Failed decoding the Codegen Output\n" <> err
          exitFailure
        Right res -> return res
    ".textproto" -> do
      content <- LText.readFile fp
      return $ PbText.readMessageOrDie content
    _ -> do
      logError fp $ "Unknown Codegen Output format (wanted .pb or .textproto) " <> ext
      exitFailure
