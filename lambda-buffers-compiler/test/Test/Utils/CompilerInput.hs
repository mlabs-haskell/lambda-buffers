module Test.Utils.CompilerInput (compilerInput'incoherent, compilerInput'maybe, compilerInput'doubleDeclarationDiffMod, compilerInput'doubleDeclaration) where

import Control.Lens ((%~), (&))
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Utils.Constructors (_CompilerInput)
import Test.Utils.Module (module'incoherent, module'maybe)
import Test.Utils.SourceInfo (sourceInfo'empty)

-- | Compiler Input containing 1 module with 1 definition - Maybe.
compilerInput'maybe :: P.CompilerInput
compilerInput'maybe = _CompilerInput [module'maybe]

-- | Contains 2 definitions - 1 wrong one.
compilerInput'incoherent :: P.CompilerInput
compilerInput'incoherent = _CompilerInput [module'maybe, module'incoherent]

-- | Declares maybe twice.
compilerInput'doubleDeclaration :: P.CompilerInput
compilerInput'doubleDeclaration = compilerInput'maybe <> compilerInput'maybe

-- | Declares maybe twice - in different modules
compilerInput'doubleDeclarationDiffMod :: P.CompilerInput
compilerInput'doubleDeclarationDiffMod =
  compilerInput'maybe
    <> _CompilerInput
      [ module'maybe & #moduleName . #parts %~ (P.ModuleNamePart "Module" sourceInfo'empty :)
      ]
