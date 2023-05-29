module Test.Utils.CompilerInput (
  compilerInput'incoherent,
  compilerInput'maybe,
  compilerInput'undefinedVariable,
  compilerInput'undefinedLocalTyRef,
  compilerInput'either,
  compilerInput'undefinedForeignTyRef,
  compilerInput'recDef,
  compilerInput'newTypeEither,
  compilerInput'newTypeEither',
  compilerInput'newTypeEither'',
) where

import LambdaBuffers.ProtoCompat qualified as PC
import Test.Utils.Constructors (_CompilerInput)
import Test.Utils.Module (
  module'either,
  module'incoherent,
  module'maybe,
  module'newTypeEither,
  module'newTypeEither',
  module'newTypeEither'',
  module'recDef,
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

compilerInput'newTypeEither :: PC.CompilerInput
compilerInput'newTypeEither = _CompilerInput [module'newTypeEither]

compilerInput'newTypeEither' :: PC.CompilerInput
compilerInput'newTypeEither' = _CompilerInput [module'newTypeEither']

compilerInput'newTypeEither'' :: PC.CompilerInput
compilerInput'newTypeEither'' = _CompilerInput [module'newTypeEither'']

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
