module LambdaBuffers.Codegen.Cli.GenPurescript (GenOpts (..), gen) where

import Control.Lens (makeLenses, (^.))
import Data.Aeson (decodeFileStrict)
import LambdaBuffers.Codegen.Cli.Gen qualified as Gen
import LambdaBuffers.Codegen.Purescript (runPrint)
import LambdaBuffers.Codegen.Purescript.Config qualified as H
import Paths_lambda_buffers_codegen qualified as Paths

data GenOpts = MkGenOpts
  { _common :: Gen.GenOpts
  , _config :: Maybe FilePath
  }

makeLenses 'MkGenOpts

gen :: GenOpts -> IO ()
gen opts = do
  cfgFp <- maybe (Paths.getDataFileName "data/purescript.json") pure (opts ^. config)
  cfg <- readPurescriptConfig cfgFp

  Gen.gen
    (opts ^. common)
    (\ci -> runPrint cfg ci <$> ci ^. #modules)

readPurescriptConfig :: FilePath -> IO H.Config
readPurescriptConfig f = do
  mayCfg <- decodeFileStrict f
  case mayCfg of
    Nothing -> error $ "Invalid Purescript configuration file " <> f
    Just cfg -> return cfg
