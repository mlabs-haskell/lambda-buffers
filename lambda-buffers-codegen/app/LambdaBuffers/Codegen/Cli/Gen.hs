module LambdaBuffers.Codegen.Cli.Gen (GenOpts (..), writeFileAndCreate, inputFile, outputFile, debug, gen) where

import Control.Lens (makeLenses, (&), (.~), (^.))
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Foldable (Foldable (fold), foldrM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.ProtoLens qualified as Pb
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy.IO qualified as LText
import LambdaBuffers.Compiler.TypeClassCheck.Errors qualified as PC
import LambdaBuffers.ProtoCompat qualified as PC
import Proto.Codegen qualified as P
import Proto.Codegen_Fields qualified as P
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

gen :: GenOpts -> (PC.CodegenInput -> Map (PC.InfoLess PC.ModuleName) (Either P.Error (FilePath, Text))) -> IO ()
gen opts cont = do
  putStrLn $ "Code generation Input at " <> opts ^. inputFile
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
              return (err : errs)
            Right (fp, printed) -> do
              putStrLn $
                "Code generation succeeded for module "
                  <> PC.withInfoLess mn (show . PC.prettyModuleName)
                  <> " at file path "
                  <> (opts ^. printDir </> fp)
              writeFileAndCreate (opts ^. printDir </> fp) printed
              return errs
      )
      []
      (Map.toList res)

  if null allErrors
    then do
      writeCodegenResult (opts ^. outputFile)
      putStrLn "Code generation successful."
    else do
      writeCodegenError (opts ^. outputFile) allErrors
      error "Code generation reported errors"
  putStrLn $ "Code generation Output at " <> opts ^. outputFile

runFromProto :: FilePath -> P.Input -> IO PC.CodegenInput
runFromProto ofp ci = case PC.codegenInputFromProto ci of
  Left err -> do
    writeCodegenError ofp [err]
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

readCodegenInput :: FilePath -> IO P.Input
readCodegenInput fp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> do
      content <- BS.readFile fp
      return $ Pb.decodeMessageOrDie content
    ".textproto" -> do
      content <- LText.readFile fp
      return $ PbText.readMessageOrDie content
    _ -> error $ "Unknown Codegen Input format (wanted .pb or .textproto) " <> ext

writeCodegenError :: FilePath -> [P.Error] -> IO ()
writeCodegenError fp err = writeCodegenOutput fp (defMessage & P.error .~ fold err)

writeCodegenResult :: FilePath -> IO ()
writeCodegenResult fp = writeCodegenOutput fp defMessage

writeCodegenOutput :: FilePath -> P.Output -> IO ()
writeCodegenOutput fp cr = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> BS.writeFile fp (Pb.encodeMessage cr)
    ".textproto" -> Text.writeFile fp (Text.pack . show $ PbText.pprintMessage cr)
    _ -> error $ "Unknown Codegen Output format (wanted .pb or .textproto) " <> ext
