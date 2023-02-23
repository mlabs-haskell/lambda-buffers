module LambdaBuffers.Compiler.Cli.Compile (CompileOpts (..), compile) where

import Control.Lens (makeLenses, (^.))
import Data.ByteString qualified as BS
import Data.ProtoLens qualified as Pb
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.IO qualified as Text
import LambdaBuffers.Compiler (runCompiler)
import Proto.Compiler (CompilerInput, CompilerOutput)
import Proto.Compiler_Fields (maybe'compilerError)
import System.FilePath.Lens (extension)

data CompileOpts = CompileOpts
  { _input :: FilePath
  , _output :: FilePath
  }
  deriving stock (Eq, Show)

makeLenses ''CompileOpts

-- NOTE(cstml): Let's use Katip instead of print.

-- | Compile LambdaBuffers modules
compile :: CompileOpts -> IO ()
compile opts = do
  compInp <- readCompilerInput (opts ^. input)
  let compOut = runCompiler compInp
  case compOut ^. maybe'compilerError of
    Nothing -> do
      putStrLn "Compilation succeeded"
    Just _ -> do
      putStrLn "Compilation failed"
  writeCompilerOutput (opts ^. output) compOut

readCompilerInput :: FilePath -> IO CompilerInput
readCompilerInput fp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> do
      content <- BS.readFile fp
      return $ Pb.decodeMessageOrDie content
    ".textproto" -> do
      content <- Text.readFile fp
      return $ PbText.readMessageOrDie content
    _ -> error $ "Unknown CompilerInput format " <> ext

writeCompilerOutput :: FilePath -> CompilerOutput -> IO ()
writeCompilerOutput fp cr = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> BS.writeFile fp (Pb.encodeMessage cr)
    ".textproto" -> Text.writeFile fp (Text.pack . show $ PbText.pprintMessage cr)
    _ -> error $ "Unknown CompilerOutput format " <> ext
