module LambdaBuffers.Codegen.Cli.Gen (GenOpts (..), inputFile, outputFile, debug, gen) where

import Control.Lens (makeLenses, (&), (.~), (^.))
import Data.ByteString qualified as BS
import Data.ProtoLens (Message (defMessage))
import Data.ProtoLens qualified as Pb
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.IO qualified as Text
import LambdaBuffers.Compiler.ProtoCompat.FromProto (runFromProto)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P
import System.FilePath.Lens (extension)

data GenOpts = GenOpts
  { _inputFile :: FilePath
  , _outputFile :: FilePath
  , _debug :: Bool
  }
  deriving stock (Eq, Show)

makeLenses ''GenOpts

-- TODO(bladyjoker): Create proper CodegenInput and CodegenOutput messages.
gen :: GenOpts -> (PC.CompilerInput -> IO (Either P.CompilerError P.CompilerResult)) -> IO ()
gen opts cont = do
  ci <- readCompilerInput (opts ^. inputFile)
  case runFromProto ci of
    Left err -> do
      writeCompilerOutput (opts ^. outputFile) defMessage
      error $ "Failed parsing the input " <> show err
    Right ci' -> do
      errOrRes <- cont ci'
      case errOrRes of
        Left err -> do
          writeCompilerOutput (opts ^. outputFile) (defMessage & P.compilerError .~ err)
          error $ "Generation failed, see " <> (opts ^. outputFile) <> " for details"
        Right res -> do
          writeCompilerOutput (opts ^. outputFile) (defMessage & P.compilerResult .~ res)
          putStrLn "Generation succeeded"

readCompilerInput :: FilePath -> IO P.CompilerInput
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

writeCompilerOutput :: FilePath -> P.CompilerOutput -> IO ()
writeCompilerOutput fp cr = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> BS.writeFile fp (Pb.encodeMessage cr)
    ".textproto" -> Text.writeFile fp (Text.pack . show $ PbText.pprintMessage cr)
    _ -> error $ "Unknown CompilerOutput format " <> ext
