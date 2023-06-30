module LambdaBuffers.Codegen.Cli.GenPurescript (GenOpts (..), gen) where

import Control.Lens (makeLenses, (^.))
import Control.Monad (unless)
import Data.Aeson (decodeFileStrict)
import LambdaBuffers.Codegen.Cli.Gen (logError)
import LambdaBuffers.Codegen.Cli.Gen qualified as Gen
import LambdaBuffers.Codegen.Purescript (runPrint)
import LambdaBuffers.Codegen.Purescript.Config qualified as H
import Paths_lambda_buffers_codegen qualified as Paths
import System.Directory (doesFileExist)
import System.Exit (exitFailure)

data GenOpts = MkGenOpts
  { _config :: Maybe FilePath
  , _common :: Gen.GenOpts
  }

makeLenses 'MkGenOpts

gen :: GenOpts -> IO ()
gen opts = do
  cfgFp <- maybe (Paths.getDataFileName "data/purescript.json") pure (opts ^. config)
  cfg <- readPurescriptConfig cfgFp

  Gen.gen
    (opts ^. common)
    (\ci -> fmap (\(fp, code, deps) -> Gen.Generated fp code deps) . runPrint cfg ci <$> (ci ^. #modules))

readPurescriptConfig :: FilePath -> IO H.Config
readPurescriptConfig f = do
  fExists <- doesFileExist f
  unless
    fExists
    ( do
        logError $ "Provided Purescript Codegen configuration file doesn't exists: " <> f
        exitFailure
    )
  mayCfg <- decodeFileStrict f
  case mayCfg of
    Nothing -> do
      logError $ "Invalid Purescript configuration file " <> f
      exitFailure
    Just cfg -> return cfg
