module Test.LambdaBuffers.Compiler.Coverage (coverage) where

import Control.Lens ((^.))
import Hedgehog (MonadTest, collect)
import Proto.Compiler (CompilerInput)
import Proto.Compiler_Fields (modules, typeDefs)

-- TODO(bladyjoker): Add stats on TyDef per Module, TyArgs per TyDef etc...
coverage :: MonadTest m => CompilerInput -> m ()
coverage compInp = do
  let nModules = length $ compInp ^. modules
      nTyDefs = length $ [td | m <- compInp ^. modules, td <- m ^. typeDefs]
  collect ("number of modules" :: String, nModules)
  collect ("number of type definitions" :: String, nTyDefs)
