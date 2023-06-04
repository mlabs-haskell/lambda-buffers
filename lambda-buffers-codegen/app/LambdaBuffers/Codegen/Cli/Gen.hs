module LambdaBuffers.Codegen.Cli.Gen (GenOpts (..), writeFileAndCreate, inputFile, outputFile, debug, gen, logInfo, logError) where

import Control.Lens (makeLenses, (&), (.~), (^.))
import Control.Monad (when)
import Data.Aeson (encodeFile)
import Data.ByteString qualified as BS
import Data.Foldable (Foldable (fold), foldrM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.ProtoLens qualified as Pb
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy.IO qualified as LText
import LambdaBuffers.ProtoCompat qualified as PC
import Proto.Codegen qualified as P
import Proto.Codegen_Fields qualified as P
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.FilePath.Lens (extension)

data GenOpts = GenOpts
  { _inputFile :: FilePath
  , _outputFile :: FilePath
  , _genDir :: FilePath
  , _debug :: Bool
  }
  deriving stock (Eq, Show)

makeLenses ''GenOpts

logInfo :: String -> IO ()
logInfo msg = putStrLn $ "[lbg][INFO] " <> msg <> "."

logError :: String -> IO ()
logError msg = putStrLn $ "[lbg][ERROR] " <> msg <> "."

type Handler = (PC.CodegenInput -> Map (PC.InfoLess PC.ModuleName) (Either P.Error (FilePath, Text, Set Text)))

gen :: GenOpts -> Handler -> IO ()
gen opts cont = do
  logInfo $ "Code generation Input at " <> opts ^. inputFile
  ci <- readCodegenInput (opts ^. inputFile)
  ci' <- runFromProto (opts ^. outputFile) ci
  initialisePrintDir (opts ^. genDir)
  let res = cont ci'
  (allErrors, allDeps) <- collectErrorsAndDeps opts res
  if null allErrors
    then do
      writeCodegenResult (opts ^. outputFile)
      writePackageDeps (opts ^. genDir </> "build.json") allDeps
      logInfo "Code generation successful"
    else do
      writeCodegenError (opts ^. outputFile) allErrors
      logError "Code generation reported errors"
  logInfo $ "Code generation Output at " <> opts ^. outputFile

collectErrorsAndDeps :: forall {b} {a}. Monoid b => GenOpts -> Map (PC.InfoLess PC.ModuleName) (Either a (FilePath, Text, b)) -> IO ([a], b)
collectErrorsAndDeps opts res =
  foldrM
    ( \(mn, errOrPrint) (errs, deps) -> do
        case errOrPrint of
          Left err -> do
            logInfo $
              "Code generation failed for module "
                <> PC.withInfoLess mn (show . PC.prettyModuleName)
            return (err : errs, deps)
          Right (fp, printed, deps') -> do
            logInfo $
              "Code generation succeeded for module "
                <> PC.withInfoLess mn (show . PC.prettyModuleName)
                <> " at file path "
                <> (opts ^. genDir </> fp)
            writeFileAndCreate (opts ^. genDir </> fp) printed
            return (errs, deps <> deps')
    )
    ([], mempty)
    (Map.toList res)

runFromProto :: FilePath -> P.Input -> IO PC.CodegenInput
runFromProto ofp ci = case PC.codegenInputFromProto ci of
  Left err -> do
    writeCodegenError ofp [err]
    logError $ "Code generation failed due to problems with the input file, inspect the error in " <> ofp <> " to find out the details"
    exitFailure
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
    _ -> do
      logError $ "Unknown Codegen Input format (wanted .pb or .textproto) " <> ext
      exitFailure

writeCodegenError :: FilePath -> [P.Error] -> IO ()
writeCodegenError fp errs = writeCodegenOutput fp (defMessage & P.error .~ fold errs)

writeCodegenResult :: FilePath -> IO ()
writeCodegenResult fp = writeCodegenOutput fp defMessage

writeCodegenOutput :: FilePath -> P.Output -> IO ()
writeCodegenOutput fp cr = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> BS.writeFile fp (Pb.encodeMessage cr)
    ".textproto" -> Text.writeFile fp (Text.pack . show $ PbText.pprintMessage cr)
    _ -> do
      logError $ "Unknown Codegen Output format (wanted .pb or .textproto) " <> ext
      exitFailure

writePackageDeps :: FilePath -> Set Text -> IO ()
writePackageDeps = encodeFile
