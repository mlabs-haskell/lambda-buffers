module Test.Samples.Proto.CompilerInput (compilerInput'incoherent, compilerInput'maybe, compilerInput'doubleDeclaration, compilerInput'doubleDeclarationDiffMod) where

import Control.Lens ((%~), (&), (.~))
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Samples.Proto.Module (module'incoherent, module'maybe)
import Test.Samples.Proto.SourceInfo

_CompilerInput x = P.CompilerInput {P.modules = x}

-- | Compiler Input containing 1 module with 1 definition - Maybe.
compilerInput'maybe :: P.CompilerInput
compilerInput'maybe = _CompilerInput [module'maybe]

-- | Contains 2 definitions - 1 wrong one.
compilerInput'incoherent :: P.CompilerInput
compilerInput'incoherent = compilerInput'maybe & #modules .~ [module'incoherent]

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
