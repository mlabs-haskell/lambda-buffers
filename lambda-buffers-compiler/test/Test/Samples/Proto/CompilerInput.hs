{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Test.Samples.Proto.CompilerInput where

import Control.Lens ((&), (.~))
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Samples.Proto.Module

-- | Compiler Input containing 1 module with 1 definition - Maybe.
ci1 :: P.CompilerInput
ci1 = P.CompilerInput {P.modules = [modMaybe]}

-- | Contains 2 definitions - 1 wrong one.
ci2 :: P.CompilerInput
ci2 = ci1 & #modules .~ [addMod]
