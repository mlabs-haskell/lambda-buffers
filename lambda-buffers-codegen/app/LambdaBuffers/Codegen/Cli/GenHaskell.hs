module LambdaBuffers.Codegen.Cli.GenHaskell (GenOpts (..), gen) where

import Control.Lens (makeLenses, (^.))
import Control.Monad (unless)
import Data.Aeson (decodeFileStrict')
import LambdaBuffers.Codegen.Cli.Gen (logError)
import LambdaBuffers.Codegen.Cli.Gen qualified as Gen
import LambdaBuffers.Codegen.Haskell (runPrint)
import LambdaBuffers.Codegen.Haskell.Config qualified as H
import Paths_lambda_buffers_codegen qualified as Paths
import System.Directory (doesFileExist)
import System.Directory.Internal.Prelude (exitFailure)

data GenOpts = MkGenOpts
  { _config :: [FilePath]
  , _common :: Gen.GenOpts
  }

makeLenses 'MkGenOpts

gen :: GenOpts -> IO ()
gen opts = do
  cfg <- case opts ^. config of
    [] -> do
      fp <- Paths.getDataFileName "data/haskell-prelude-base.json"
      readHaskellConfig fp
    fps -> do
      cfgs <- traverse readHaskellConfig fps
      return (mconcat cfgs)

  Gen.gen
    (opts ^. common)
    (\ci -> fmap (\(fp, code, deps) -> Gen.Generated fp code deps) . runPrint cfg ci <$> (ci ^. #modules))

readHaskellConfig :: FilePath -> IO H.Config
readHaskellConfig f = do
  fExists <- doesFileExist f
  unless
    fExists
    ( do
        logError $ "Provided Haskell Codegen configuration file doesn't exists: " <> f
        exitFailure
    )
  mayCfg <- decodeFileStrict' f
  case mayCfg of
    Nothing -> do
      logError $ "Invalid Haskell configuration file " <> f
      exitFailure
    Just cfg -> return cfg
