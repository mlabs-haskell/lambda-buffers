module LambdaBuffers.Frontend.Cli.Compile (CompileOpts (..), compile) where

import Control.Lens (makeLenses, (^.))
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.ProtoLens.Encoding qualified as Pb
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.IO qualified as Text
import LambdaBuffers.Frontend (runFrontend)
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.ToProto (toCompilerInput)
import Proto.Compiler (CompilerInput)
import System.FilePath ((<.>), (</>))
import System.FilePath.Lens (extension)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

data CompileOpts = CompileOpts
  { _importPaths :: [FilePath]
  , _moduleFilepath :: FilePath
  , _compilerCliFilepath :: FilePath
  , _debug :: Bool
  , _workingDir :: Maybe FilePath
  }
  deriving stock (Eq, Show)

makeLenses ''CompileOpts

-- | Compile a filepath containing a LambdaBuffers module
compile :: CompileOpts -> IO ()
compile opts = do
  errOrMod <- runFrontend (opts ^. importPaths) (opts ^. moduleFilepath)
  case errOrMod of
    Left err -> print err
    Right res -> do
      -- TODO(bladyjoker): Use proper logging (Katip)
      putStrLn "OK"
      compInp <- either (\e -> error $ "Failed translating to Compiler proto: " <> show e) return (toCompilerInput res)
      let ext = if opts ^. debug then "textproto" else "pb"
      withSystemTempDirectory
        "lambda-buffers-frontend"
        ( \tempDir -> do
            let workDir = fromMaybe tempDir (opts ^. workingDir)
                compInpFp = workDir </> "compiler-input" <.> ext
                compOutFp = workDir </> "compiler-output" <.> ext
            writeCompilerInput compInpFp compInp
            let args = ["compile", "--input-file", compInpFp, "--output-file", compOutFp]
            putStrLn $ "Calling: " <> show (opts ^. compilerCliFilepath) <> " on " <> show args
            callProcess (opts ^. compilerCliFilepath) args
        )

writeCompilerInput :: FilePath -> CompilerInput -> IO ()
writeCompilerInput fp compInp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> BS.writeFile fp (Pb.encodeMessage compInp)
    ".textproto" -> Text.writeFile fp (Text.pack . show $ PbText.pprintMessage compInp)
    _ -> error $ "Unknown CompilerInput format " <> ext
