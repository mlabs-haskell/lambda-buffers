module LambdaBuffers.Compiler.Cli.Compile (CompileOpts (..), compile) where

import Control.Lens (makeLenses)
import Control.Lens.Getter ((^.))
import Data.ByteString qualified as BS
import Data.ProtoLens qualified as Pb
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.IO qualified as Text
import LambdaBuffers.Compiler.ProtoCompat ()
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as ProtoCompat
import Proto.Compiler (CompilerInput)
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
  {-
  compIn <- readCompilerInput (opts ^. input)
  case fromProto @CompilerInput @ProtoCompat.CompilerInput compIn of
    Left err -> case err of
      NamingError ne -> print $ "Encountered a naming error " <> show ne
      ProtoError pe -> print $ "Encountered a proto error " <> show pe
    Right compIn' -> do
      print @String "Successfully processed the CompilerInput"
      writeCompilerOutput (opts ^. output) (toProto compIn')
  -}
  return ()

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

-- FIXME(bladyjoker): Do this properly when you figure out what the CompilerOutput is.
writeCompilerOutput :: FilePath -> CompilerInput -> IO ()
writeCompilerOutput fp co = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> BS.writeFile fp (Pb.encodeMessage co)
    ".textproto" -> Text.writeFile fp (Text.pack . show $ PbText.pprintMessage co)
    _ -> error $ "Unknown CompilerOutput format " <> ext
