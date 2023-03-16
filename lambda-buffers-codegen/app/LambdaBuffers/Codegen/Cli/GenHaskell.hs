module LambdaBuffers.Codegen.Cli.GenHaskell (GenOpts (..), gen) where

import Control.Lens (makeLenses, (^.))
import Data.Aeson (decodeFileStrict)
import LambdaBuffers.Codegen.Cli.Gen qualified as Gen
import LambdaBuffers.Codegen.Haskell (runPrint)
import LambdaBuffers.Codegen.Haskell.Config qualified as H
import Paths_lambda_buffers_codegen qualified as Paths

data GenOpts = MkGenOpts
  { _common :: Gen.GenOpts
  , _config :: Maybe FilePath
  }

makeLenses 'MkGenOpts

gen :: GenOpts -> IO ()
gen opts = do
  cfgFp <- maybe (Paths.getDataFileName "data/haskell.json") pure (opts ^. config)
  cfg <- readHaskellConfig cfgFp

  Gen.gen
    (opts ^. common)
    (\ci -> runPrint cfg <$> ci ^. #modules)

readHaskellConfig :: FilePath -> IO H.Config
readHaskellConfig f = do
  mayCfg <- decodeFileStrict f
  case mayCfg of
    Nothing -> error $ "Invalid Haskell configuration file " <> f
    Just cfg -> return cfg
