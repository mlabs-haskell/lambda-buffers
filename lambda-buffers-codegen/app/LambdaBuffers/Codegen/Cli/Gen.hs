{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Codegen.Cli.Gen (GenOpts (..), Generated (..), writeFileAndCreate, inputFile, outputFile, debug, gen, logInfo, logError) where

import Control.Lens (makeLenses, (&), (.~), (^.))
import Control.Monad (unless, when)
import Data.Aeson (encodeFile)
import Data.ByteString qualified as BS
import Data.Foldable (Foldable (fold, toList), foldrM)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.ProtoLens qualified as Pb
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy.IO qualified as LText
import Data.Traversable (for)
import LambdaBuffers.Codegen.Config qualified as Config
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
  , _requestedClasses :: [String]
  , _requestedModules :: NonEmpty String
  }
  deriving stock (Eq, Show)

makeLenses ''GenOpts

logInfo :: String -> IO ()
logInfo msg = putStrLn $ "[lbg][INFO] " <> msg <> "."

logError :: String -> IO ()
logError msg = putStrLn $ "[lbg][ERROR] " <> msg <> "."

data Generated = Generated
  { _generatedFilePath :: FilePath
  , _generatedCode :: Text
  , _generatedPackageDeps :: Set Text
  }
  deriving stock (Show, Eq, Ord)

makeLenses ''Generated

type Handler = (PC.CodegenInput -> Map (PC.InfoLess PC.ModuleName) (Either P.Error Generated))

gen :: GenOpts -> Handler -> IO ()
gen opts cont = do
  logInfo $ "Codegen Input at " <> opts ^. inputFile
  when (opts ^. debug) $ logInfo $ "Options received: " <> show opts
  ci <- readCodegenInput (opts ^. inputFile)
  ci' <- runFromProto (opts ^. outputFile) ci
  ci'' <- filterToRequestedClasses' opts ci'
  initialisePrintDir (opts ^. genDir)
  let res = cont ci''
  (allErrors, allDeps) <- collectErrorsAndDeps opts res
  if null allErrors
    then do
      writeCodegenResult (opts ^. outputFile)
      writePackageDeps (opts ^. genDir </> "build.json") allDeps
      logInfo "Code generation successful"
    else do
      writeCodegenError (opts ^. outputFile) allErrors
      logError "Code generation reported errors"
  logInfo $ "Codegen Output at " <> opts ^. outputFile

instance MonadFail (Either String) where
  fail = Left

filterToRequestedClasses' :: GenOpts -> PC.CodegenInput -> IO PC.CodegenInput
filterToRequestedClasses' opts ci = do
  reqCls <-
    for
      (toList $ opts ^. requestedClasses)
      ( \cl -> do
          case Config.qClassNameFromText . Text.pack $ cl of
            Left err -> do
              logError err
              exitFailure
            Right qcn -> return qcn
      )
  filterToRequestedClasses (Set.fromList reqCls) ci

filterToRequestedClasses :: Set PC.QClassName -> PC.CodegenInput -> IO PC.CodegenInput
filterToRequestedClasses reqCls ci =
  let
    ciClassRels = PC.indexClassRelations ci
    ciQClassNames = Map.keysSet ciClassRels
    requestedClasses' = classClosure ciClassRels reqCls
   in
    do
      logInfo $ "Computed class closure: " <> unwords (Text.unpack . Config.qClassNameToText <$> toList reqCls)
      unless (null (reqCls `Set.difference` ciQClassNames)) $ do
        logError $
          "Requested to print classes that are not available in the provided context."
            <> "\nClasses requested: "
            <> unwords (Text.unpack . Config.qClassNameToText <$> toList reqCls)
            <> "\nClasses available: "
            <> unwords (Text.unpack . Config.qClassNameToText <$> toList ciQClassNames)
            <> "\nClasses missing: "
            <> unwords (Text.unpack . Config.qClassNameToText <$> toList (reqCls `Set.difference` ciQClassNames))
        exitFailure
      return $ ci & #modules .~ (filterClassInModule requestedClasses' <$> ci ^. #modules)
  where
    classClosure :: PC.ClassRels -> Set PC.QClassName -> Set PC.QClassName
    classClosure classRels cls =
      let classRels' = Map.filterWithKey (\k _x -> k `Set.member` cls) classRels
          cls' = cls <> (Set.fromList . mconcat . Map.elems $ classRels')
       in if cls == cls'
            then cls
            else classClosure classRels cls'
    filterClassInModule :: Set PC.QClassName -> PC.Module -> PC.Module
    filterClassInModule cls m =
      m
        { PC.classDefs = Map.filterWithKey (\_clName clDef -> PC.qualifyClassName (m ^. #moduleName) (clDef ^. #className) `Set.member` cls) (m ^. #classDefs)
        , PC.instances = [i | i <- m ^. #instances, filterInstance cls m i]
        , PC.derives = [d | d <- m ^. #derives, filterDerive cls m d]
        }
    filterInstance :: Set PC.QClassName -> PC.Module -> PC.InstanceClause -> Bool
    filterInstance cls m inst = filterConstraint cls m (inst ^. #head)
    filterDerive :: Set PC.QClassName -> PC.Module -> PC.Derive -> Bool
    filterDerive cls m drv = filterConstraint cls m (drv ^. #constraint)
    filterConstraint :: Set PC.QClassName -> PC.Module -> PC.Constraint -> Bool
    filterConstraint cls m cnstr = PC.qualifyClassRef (m ^. #moduleName) (cnstr ^. #classRef) `Set.member` cls

filterToRequestedModules :: GenOpts -> Map (PC.InfoLess PC.ModuleName) (Either P.Error Generated) -> IO (Map (PC.InfoLess PC.ModuleName) (Either P.Error Generated))
filterToRequestedModules opts res = do
  let uniqMns = toList $ NonEmpty.nub $ opts ^. requestedModules
  onlyModules <- (Config.moduleNameFromText . Text.pack) `traverse` uniqMns
  return $ Map.restrictKeys res (Set.fromList onlyModules)

collectErrorsAndDeps :: GenOpts -> Map (PC.InfoLess PC.ModuleName) (Either P.Error Generated) -> IO ([P.Error], Set Text)
collectErrorsAndDeps opts res = do
  res' <- filterToRequestedModules opts res
  foldrM
    ( \(mn, errOrPrint) (errs, deps) -> do
        case errOrPrint of
          Left err -> do
            logInfo $
              "Code generation failed for module "
                <> PC.withInfoLess mn (show . PC.prettyModuleName)
            return (err : errs, deps)
          Right gend -> do
            writeFileAndCreate (opts ^. genDir </> (gend ^. generatedFilePath)) (gend ^. generatedCode)
            logInfo $
              "Code generation succeeded for module "
                <> PC.withInfoLess mn (show . PC.prettyModuleName)
                <> " at file path "
                <> (opts ^. genDir </> (gend ^. generatedFilePath))
            return (errs, deps <> gend ^. generatedPackageDeps)
    )
    ([], mempty)
    (Map.toList res')

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
