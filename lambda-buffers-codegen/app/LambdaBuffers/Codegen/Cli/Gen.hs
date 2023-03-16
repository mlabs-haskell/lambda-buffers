module LambdaBuffers.Codegen.Cli.Gen (GenOpts (..), writeFileAndCreate, inputFile, outputFile, debug, gen) where

import Control.Lens (makeLenses, (&), (.~), (^.))
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Foldable (foldrM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.ProtoLens qualified as Pb
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy.IO qualified as LText
import LambdaBuffers.Compiler.ProtoCompat.FromProto qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Utils qualified as PC
import LambdaBuffers.Compiler.TypeClassCheck.Errors qualified as PC
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath (takeDirectory, (</>))
import System.FilePath.Lens (extension)

data GenOpts = GenOpts
  { _inputFile :: FilePath
  , _outputFile :: FilePath
  , _printDir :: FilePath
  , _debug :: Bool
  }
  deriving stock (Eq, Show)

makeLenses ''GenOpts

-- TODO(bladyjoker): Create proper CodegenInput and CodegenOutput messages.
gen :: GenOpts -> (PC.CompilerInput -> Map (PC.InfoLess PC.ModuleName) (Either P.CompilerError (FilePath, Text))) -> IO ()
gen opts cont = do
  ci <- readCodegenInput (opts ^. inputFile)
  ci' <- runFromProto (opts ^. outputFile) ci
  initialisePrintDir (opts ^. printDir)
  let res = cont ci'
  allErrors <-
    foldrM
      ( \(mn, errOrPrint) errs -> do
          case errOrPrint of
            Left err -> do
              putStrLn $
                "Code generation failed for module "
                  <> PC.withInfoLess mn (show . PC.prettyModuleName)
              return (err `PC.mappendErrs` errs)
            Right (fp, printed) -> do
              putStrLn $
                "Code generation succeeded for module "
                  <> PC.withInfoLess mn (show . PC.prettyModuleName)
                  <> " at file path "
                  <> (opts ^. printDir </> fp)
              writeFileAndCreate (opts ^. printDir </> fp) printed
              return errs
      )
      PC.memptyErr
      (Map.toList res)

  if allErrors == PC.memptyErr
    then putStrLn "Code generation successful."
    else do
      writeCodegenError (opts ^. outputFile) allErrors
      error "Code generation reported errors"

runFromProto :: FilePath -> P.CompilerInput -> IO PC.CompilerInput
runFromProto ofp ci = case PC.runFromProto ci of
  Left err -> do
    writeCodegenError ofp err
    error $ "Code generation failed due to problem with the input file, inspect the error in " <> ofp <> " to find out the details"
  Right ci' -> return ci'

writeFileAndCreate :: FilePath -> Text -> IO ()
writeFileAndCreate fp printed = do
  let printedDir = takeDirectory fp
  createDirectoryIfMissing True printedDir
  Text.writeFile fp printed

initialisePrintDir :: FilePath -> IO ()
initialisePrintDir fp = do
  exists <- doesDirectoryExist fp
  when exists (removeDirectoryRecursive fp)
  createDirectory fp

readCodegenInput :: FilePath -> IO P.CompilerInput
readCodegenInput fp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> do
      content <- BS.readFile fp
      return $ Pb.decodeMessageOrDie content
    ".textproto" -> do
      content <- LText.readFile fp
      return $ PbText.readMessageOrDie content
    _ -> error $ "Unknown CompilerInput format " <> ext

writeCodegenError :: FilePath -> P.CompilerError -> IO ()
writeCodegenError fp err = writeCodegenOutput fp (defMessage & P.compilerError .~ err)

writeCodegenOutput :: FilePath -> P.CompilerOutput -> IO ()
writeCodegenOutput fp cr = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> BS.writeFile fp (Pb.encodeMessage cr)
    ".textproto" -> Text.writeFile fp (Text.pack . show $ PbText.pprintMessage cr)
    _ -> error $ "Unknown CompilerOutput format " <> ext
