module Test.Utils.CompilerInput (compilerInput'incoherent, compilerInput'maybe) where

import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Utils.Constructors (_CompilerInput)
import Test.Utils.Module (module'incoherent, module'maybe)

-- | Compiler Input containing 1 module with 1 definition - Maybe.
compilerInput'maybe :: P.CompilerInput
compilerInput'maybe = _CompilerInput [module'maybe]

-- | Contains 2 definitions - 1 wrong one.
compilerInput'incoherent :: P.CompilerInput
compilerInput'incoherent = _CompilerInput [module'maybe, module'incoherent]
