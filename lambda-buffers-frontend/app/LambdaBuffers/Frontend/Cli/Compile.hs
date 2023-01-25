module LambdaBuffers.Frontend.Cli.Compile (CompileOpts (..), compile) where

import Control.Lens (makeLenses, (^.))
import Data.Map qualified as Map
import LambdaBuffers.Frontend (runFrontend)
import LambdaBuffers.Frontend.PPrint ()
import Prettyprinter (Pretty (pretty))
import Proto.Compiler ()

data CompileOpts = CompileOpts
  { _importPaths :: [FilePath]
  , _moduleFilepath :: FilePath
  }
  deriving stock (Eq, Show)

makeLenses ''CompileOpts

-- | Compile a filepath containing a LambdaBuffers module
compile :: CompileOpts -> IO ()
compile opts = do
  errOrMod <- runFrontend (opts ^. importPaths) (opts ^. moduleFilepath)
  case errOrMod of
    Left err -> print err
    Right mods -> do
      putStrLn "OK"
      putStrLn $ "Compiler closure contains the following modules: " <> (show . pretty . Map.elems $ mods)
