module LambdaBuffers.Codegen.Cli.GenPlutarch (GenOpts (..), gen) where

import Control.Lens (makeLenses, (^.))
import Control.Monad (unless)
import Data.Aeson (decodeFileStrict')
import LambdaBuffers.Codegen.Cli.Gen (logError)
import LambdaBuffers.Codegen.Cli.Gen qualified as Gen
import LambdaBuffers.Codegen.Haskell.Config qualified as Haskell
import LambdaBuffers.Codegen.Plutarch qualified as Plutarch
import System.Directory (doesFileExist)
import System.Directory.Internal.Prelude (exitFailure)

data GenOpts = MkGenOpts
  { _config :: ![FilePath]
  , _common :: !Gen.GenOpts
  }

makeLenses 'MkGenOpts

gen :: GenOpts -> IO ()
gen opts = do
  cfg <- case opts ^. config of
    [] -> do
      logError "" "No Plutarch configuration file given"
      exitFailure
    fps -> do
      cfgs <- traverse readPlutarchConfig fps
      return (mconcat cfgs)

  Gen.gen
    (opts ^. common)
    (\ci -> fmap (\(fp, code, deps) -> Gen.Generated fp code deps) . Plutarch.runBackend cfg ci <$> (ci ^. #modules))

readPlutarchConfig :: FilePath -> IO Haskell.Config
readPlutarchConfig f = do
  fExists <- doesFileExist f
  unless
    fExists
    ( do
        logError "" $ "Provided Plutarch Codegen configuration file doesn't exists: " <> f
        exitFailure
    )
  mayCfg <- decodeFileStrict' f
  case mayCfg of
    Nothing -> do
      logError "" $ "Invalid Plutarch configuration file " <> f
      exitFailure
    Just cfg -> return cfg
