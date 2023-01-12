module LambdaBuffers.Frontend.Cli.Compile (CompileOpts (..), compile) where

import Control.Lens (makeLenses)

newtype CompileOpts = CompileOpts
  { _importPaths :: [FilePath]
  }
  deriving stock (Eq, Show)

compile :: CompileOpts -> IO ()
compile _opts = undefined

makeLenses ''CompileOpts
