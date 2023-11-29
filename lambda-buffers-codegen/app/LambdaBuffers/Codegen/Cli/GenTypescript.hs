module LambdaBuffers.Codegen.Cli.GenTypescript (GenOpts (..), gen) where

import Control.Lens (makeLenses, (^.))
import Control.Monad (unless)
import Data.Aeson (decodeFileStrict)
import LambdaBuffers.Codegen.Cli.Gen (logError)
import LambdaBuffers.Codegen.Cli.Gen qualified as Gen
import LambdaBuffers.Codegen.Typescript (runPrint)
import LambdaBuffers.Codegen.Typescript.Config qualified as H
import Paths_lambda_buffers_codegen qualified as Paths
import System.Directory (doesFileExist)
import System.Exit (exitFailure)

data GenOpts = MkGenOpts
  { _config :: [FilePath]
  , _common :: Gen.GenOpts
  }

makeLenses 'MkGenOpts

gen :: GenOpts -> IO ()
gen opts = do
  cfg <- case opts ^. config of
    [] -> do
      fp <- Paths.getDataFileName "data/typescript-prelude-base.json"
      readTypescriptConfig fp
    fps -> do
      cfgs <- traverse readTypescriptConfig fps
      return (mconcat cfgs)

  Gen.gen
    (opts ^. common)
    (\ci -> fmap (\(fp, code, deps) -> Gen.Generated fp code deps) . runPrint cfg ci <$> (ci ^. #modules))

readTypescriptConfig :: FilePath -> IO H.Config
readTypescriptConfig f = do
  fExists <- doesFileExist f
  unless
    fExists
    ( do
        logError "" $ "Provided Typescript Codegen configuration file doesn't exists: " <> f
        exitFailure
    )
  mayCfg <- decodeFileStrict f
  case mayCfg of
    Nothing -> do
      logError "" $ "Invalid Typescript configuration file " <> f
      exitFailure
    Just cfg -> return cfg
