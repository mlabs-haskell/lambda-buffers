module LambdaBuffers.Frontend.Cli.Compile (CompileOpts (..), compile) where

import Control.Lens (makeLenses, (^.))
import Data.Map qualified as Map
import LambdaBuffers.Frontend.FrontM (runFrontM)

data CompileOpts = CompileOpts
  { _importPaths :: [FilePath]
  , _moduleFilepath :: FilePath
  }
  deriving stock (Eq, Show)

makeLenses ''CompileOpts

compile :: CompileOpts -> IO ()
compile opts = do
  errOrMod <- runFrontM (opts ^. importPaths) (opts ^. moduleFilepath)
  case errOrMod of
    Left err -> do
      print "Frontend error"
      print err
    Right mods -> do
      print "OK"
      print $ "Compiler closure contains the following modules" <> show (Map.keys mods)
