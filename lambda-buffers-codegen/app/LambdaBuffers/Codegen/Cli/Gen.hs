{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module LambdaBuffers.Codegen.Cli.Gen (GenOpts (..), gen) where

import Control.Lens (makeLenses)

data GenOpts = GenOpts
  { _compiled :: FilePath
  , _debug :: Bool
  , _workingDir :: Maybe FilePath
  }
  deriving stock (Eq, Show)

makeLenses ''GenOpts

-- | Generate code given some options.
gen :: GenOpts -> IO ()
gen _opts = error "not implemented"
