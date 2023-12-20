module LambdaBuffers.Codegen.Cli.GenRust (GenOpts (..), gen) where

import Control.Lens (makeLenses, (^.))
import Control.Monad (unless)
import Data.Aeson (decodeFileStrict')
import LambdaBuffers.Codegen.Cli.Gen (logError)
import LambdaBuffers.Codegen.Cli.Gen qualified as Gen
import LambdaBuffers.Codegen.Rust (runPrint)
import LambdaBuffers.Codegen.Rust.Config qualified as R
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
      fp <- Paths.getDataFileName "data/rust-prelude-base.json"
      readRustConfig fp
    fps -> do
      cfgs <- traverse readRustConfig fps
      return (mconcat cfgs)

  Gen.gen
    (opts ^. common)
    (\ci -> fmap (\(fp, code, deps) -> Gen.Generated fp code deps) . runPrint cfg ci <$> (ci ^. #modules))

readRustConfig :: FilePath -> IO R.Config
readRustConfig f = do
  fExists <- doesFileExist f
  unless
    fExists
    ( do
        logError "" $ "Provided Rust Codegen configuration file doesn't exists: " <> f
        exitFailure
    )
  mayCfg <- decodeFileStrict' f
  case mayCfg of
    Nothing -> do
      logError "" $ "Invalid Rust configuration file " <> f
      exitFailure
    Just cfg -> return cfg
