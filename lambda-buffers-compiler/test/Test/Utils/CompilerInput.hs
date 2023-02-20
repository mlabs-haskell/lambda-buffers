module Test.Utils.CompilerInput (
  compilerInput'incoherent,
  compilerInput'maybe,
  compilerInput'undefinedVariable,
  compilerInput'undefinedLocalTyRef,
  compilerInput'undefinedForeignTyRef,
) where

import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Utils.Constructors (_CompilerInput)
import Test.Utils.Module (module'incoherent, module'maybe, module'undefinedForeignTyRef, module'undefinedLocalTyRef, module'undefinedVar)

-- | Compiler Input containing 1 module with 1 definition - Maybe.
compilerInput'maybe :: P.CompilerInput
compilerInput'maybe = _CompilerInput [module'maybe]

-- | Contains 2 definitions - 1 wrong one.
compilerInput'incoherent :: P.CompilerInput
compilerInput'incoherent = _CompilerInput [module'maybe, module'incoherent]

-- | Contains 1 undefined variable.
compilerInput'undefinedVariable :: P.CompilerInput
compilerInput'undefinedVariable = _CompilerInput [module'undefinedVar]

-- | Contains 1 undefined Local TyRef.
compilerInput'undefinedLocalTyRef :: P.CompilerInput
compilerInput'undefinedLocalTyRef = _CompilerInput [module'undefinedLocalTyRef]

-- | Contains 1 undefined Foreign TyRef.
compilerInput'undefinedForeignTyRef :: P.CompilerInput
compilerInput'undefinedForeignTyRef = _CompilerInput [module'undefinedForeignTyRef]
