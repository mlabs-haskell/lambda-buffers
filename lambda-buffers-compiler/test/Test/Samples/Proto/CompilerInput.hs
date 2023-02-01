module Test.Samples.Proto.CompilerInput (compilerInput'incoherent, compilerInput'maybe) where

import Control.Lens ((&), (.~))
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Samples.Proto.Module (module'incoherent, module'maybe)

-- | Compiler Input containing 1 module with 1 definition - Maybe.
compilerInput'maybe :: P.CompilerInput
compilerInput'maybe = P.CompilerInput {P.modules = [module'maybe]}

-- | Contains 2 definitions - 1 wrong one.
compilerInput'incoherent :: P.CompilerInput
compilerInput'incoherent = compilerInput'maybe & #modules .~ [module'incoherent]
