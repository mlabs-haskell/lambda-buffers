module LambdaBuffers.Frontend.Cli.Compile (CompileOpts (..), compile) where

import Control.Lens (makeLenses, (^.))
import Data.Map qualified as Map
import LambdaBuffers.Frontend.FrontM (runFrontM)
import LambdaBuffers.Frontend.PPrint ()
import Prettyprinter (Pretty (pretty))
import Proto.Compiler ()

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
    Left err -> print err
    Right mods -> do
      putStrLn "OK"
      putStrLn $ "Compiler closure contains the following modules: " <> (show . pretty $ Map.keys mods)
