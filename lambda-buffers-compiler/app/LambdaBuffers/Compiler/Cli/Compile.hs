module LambdaBuffers.Compiler.Cli.Compile (CompileOpts (..), compile) where

import Control.Lens (makeLenses)
import Proto.Compiler ()

data CompileOpts = CompileOpts
  { _input :: FilePath
  , _output :: FilePath
  }
  deriving stock (Eq, Show)

makeLenses ''CompileOpts

-- | Compile LambdaBuffers modules
compile :: CompileOpts -> IO ()
compile _opts = return ()
