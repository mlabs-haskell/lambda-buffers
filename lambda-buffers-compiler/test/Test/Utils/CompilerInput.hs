module Test.Utils.CompilerInput (
  compilerInput'incoherent,
  compilerInput'maybe,
  compilerInput'undefinedVariable,
  compilerInput'undefinedLocalTyRef,
  compilerInput'either,
  compilerInput'undefinedForeignTyRef,
  compilerInput'recDef,
  compilerInput'classEq,
  compilerInput'classOrd,
  compilerInput'unboundEq,
  compilerInput'IntEqInstance,
) where

import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import Test.Utils.Constructors (_CompilerInput)
import Test.Utils.Module (
  module'IntEqInstance,
  module'classEq,
  module'classOrd,
  module'either,
  module'incoherent,
  module'maybe,
  module'recDef,
  module'unboundEq,
  module'undefinedForeignTyRef,
  module'undefinedLocalTyRef,
  module'undefinedVar,
 )

-- | Compiler Input containing 1 module with 1 definition - Maybe.
compilerInput'maybe :: PC.CompilerInput
compilerInput'maybe = _CompilerInput [module'maybe]

-- | Compiler Input containing 1 module with 1 definition - Either.
compilerInput'either :: PC.CompilerInput
compilerInput'either = _CompilerInput [module'either]

-- | Compiler Input containing 1 module with 1 definition - Either.
compilerInput'recDef :: PC.CompilerInput
compilerInput'recDef = _CompilerInput [module'recDef]

-- | Contains 2 definitions - 1 wrong one.
compilerInput'incoherent :: PC.CompilerInput
compilerInput'incoherent = _CompilerInput [module'maybe, module'incoherent]

-- | Contains 1 undefined variable.
compilerInput'undefinedVariable :: PC.CompilerInput
compilerInput'undefinedVariable = _CompilerInput [module'undefinedVar]

-- | Contains 1 undefined Local TyRef.
compilerInput'undefinedLocalTyRef :: PC.CompilerInput
compilerInput'undefinedLocalTyRef = _CompilerInput [module'undefinedLocalTyRef]

-- | Contains 1 undefined Foreign TyRef.
compilerInput'undefinedForeignTyRef :: PC.CompilerInput
compilerInput'undefinedForeignTyRef = _CompilerInput [module'undefinedForeignTyRef]

compilerInput'classEq :: PC.CompilerInput
compilerInput'classEq = _CompilerInput [module'classEq]

compilerInput'classOrd :: PC.CompilerInput
compilerInput'classOrd = _CompilerInput [module'classOrd]

compilerInput'unboundEq :: PC.CompilerInput
compilerInput'unboundEq = _CompilerInput [module'unboundEq]

compilerInput'IntEqInstance :: PC.CompilerInput
compilerInput'IntEqInstance = _CompilerInput [module'classEq, module'IntEqInstance]
