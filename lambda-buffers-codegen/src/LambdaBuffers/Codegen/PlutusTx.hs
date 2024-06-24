module LambdaBuffers.Codegen.PlutusTx (
  runBackend,
) where

import Data.Set (Set)
import Data.Text (Text)
import LambdaBuffers.Codegen.Haskell.Backend qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Backend.PlutusTx (PlutusTxHaskellBackend)
import LambdaBuffers.Codegen.Haskell.Config qualified as Haskell
import LambdaBuffers.ProtoCompat.Types qualified as PC
import Proto.Codegen qualified as P

{- | `runBackend cfg inp mod` prints a LambdaBuffers checked module `mod`, given its entire compilation closure in `inp` and Haskell configuration file in `cfg`.
  It either errors with an API error message or succeeds with a module filepath, code and package dependencies.
-}
runBackend :: Haskell.Config -> PC.CodegenInput -> PC.Module -> Either P.Error (FilePath, Text, Set Text)
runBackend = Haskell.runBackend @PlutusTxHaskellBackend () ()
