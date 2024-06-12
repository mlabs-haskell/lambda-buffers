module LambdaBuffers.Codegen.Cli.GenPlutusTx (GenOpts (..), gen) where

import Control.Lens (makeLenses, (^.))
import Control.Monad (unless)
import Data.Aeson (decodeFileStrict')
import LambdaBuffers.Codegen.Cli.Gen (logError)
import LambdaBuffers.Codegen.Cli.Gen qualified as Gen
import LambdaBuffers.Codegen.Haskell.Config qualified as Haskell
import LambdaBuffers.Codegen.PlutusTx qualified as PlutusTx
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
      logError "" "No PlutusTx configuration file given"
      exitFailure
    fps -> do
      cfgs <- traverse readPlutusTxConfig fps
      return (mconcat cfgs)

  Gen.gen
    (opts ^. common)
    (\ci -> fmap (\(fp, code, deps) -> Gen.Generated fp code deps) . PlutusTx.runBackend cfg ci <$> (ci ^. #modules))

readPlutusTxConfig :: FilePath -> IO Haskell.Config
readPlutusTxConfig f = do
  fExists <- doesFileExist f
  unless
    fExists
    ( do
        logError "" $ "Provided PlutusTx Codegen configuration file doesn't exists: " <> f
        exitFailure
    )
  mayCfg <- decodeFileStrict' f
  case mayCfg of
    Nothing -> do
      logError "" $ "Invalid PlutusTx configuration file " <> f
      exitFailure
    Just cfg -> return cfg
