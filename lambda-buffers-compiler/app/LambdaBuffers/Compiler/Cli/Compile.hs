module LambdaBuffers.Compiler.Cli.Compile (CompileOpts (..), compile) where

import Control.Lens (makeLenses, (^.))
import Data.ByteString qualified as BS
import Data.ProtoLens qualified as Pb
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.IO qualified as Text
import LambdaBuffers.Compiler (runCompiler)
import LambdaBuffers.Logger (logError, logInfo)
import Proto.Compiler (Input, Output)
import Proto.Compiler_Fields (maybe'error)
import System.Exit (exitFailure)
import System.FilePath.Lens (extension)

data CompileOpts = CompileOpts
  { _input :: FilePath
  , _output :: FilePath
  }
  deriving stock (Eq, Show)

makeLenses ''CompileOpts

-- | Compile LambdaBuffers modules
compile :: CompileOpts -> IO ()
compile opts = do
  logInfo (opts ^. input) $ "reading Compiler Input from " <> (opts ^. input)
  compInp <- readCompilerInput (opts ^. input)
  let compOut = runCompiler compInp
  case compOut ^. maybe'error of
    Nothing -> do
      logInfo (opts ^. input) "compilation succeeded"
    Just _ -> do
      logError (opts ^. input) "compilation failed"
  logInfo (opts ^. output) $ "writing Compiler Output at " <> (opts ^. output)
  writeCompilerOutput (opts ^. output) compOut

readCompilerInput :: FilePath -> IO Input
readCompilerInput fp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> do
      content <- BS.readFile fp
      return $ Pb.decodeMessageOrDie content
    ".textproto" -> do
      content <- Text.readFile fp
      return $ PbText.readMessageOrDie content
    _ -> do
      logError fp $ "unknown Compiler Input format, wanted .pb or .textproto but got " <> ext <> " (" <> fp <> ")"
      exitFailure

writeCompilerOutput :: FilePath -> Output -> IO ()
writeCompilerOutput fp cr = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> BS.writeFile fp (Pb.encodeMessage cr)
    ".textproto" -> Text.writeFile fp (Text.pack . show $ PbText.pprintMessage cr)
    _ -> do
      logError fp $ "unknown Codegen Input format, wanted .pb or .textproto but got " <> ext <> " (" <> fp <> ")"
      exitFailure
