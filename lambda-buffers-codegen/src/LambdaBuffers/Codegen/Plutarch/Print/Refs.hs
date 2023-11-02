module LambdaBuffers.Codegen.Plutarch.Print.Refs (
  plutusTypeQClassName,
  pconMethod,
  pmatchMethod,
  peqQClassName,
  peqMethod,
  pisDataQClassName,
  ptryFromQClassName,
  ptryFromMethod,
  pconQValName,
  pappQValName,
  pdataQValName,
  peqQValName,
  punsafeCoerceQValName,
  pdataQTyName,
  constQTyName,
  pasDataQTyName,
  ptryFromPAsDataQValName,
  termQTyName,
  scopeQTyName,
  ptypeQTyName,
) where

import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as HsSyntax

plutusTypeQClassName :: HsSyntax.QClassName
plutusTypeQClassName = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Internal.PlutusType", HsSyntax.MkClassName "PlutusType")

pconMethod :: HsSyntax.ValueName
pconMethod = HsSyntax.MkValueName "pcon'"

pmatchMethod :: HsSyntax.ValueName
pmatchMethod = HsSyntax.MkValueName "pmatch'"

peqQClassName :: HsSyntax.QClassName
peqQClassName = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Bool", HsSyntax.MkClassName "PEq")

peqMethod :: HsSyntax.ValueName
peqMethod = HsSyntax.MkValueName "#=="

pisDataQClassName :: HsSyntax.QClassName
pisDataQClassName = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Builtin", HsSyntax.MkClassName "PIsData")

ptryFromQClassName :: HsSyntax.QClassName
ptryFromQClassName = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.TryFrom", HsSyntax.MkClassName "PTryFrom")

ptryFromMethod :: HsSyntax.ValueName
ptryFromMethod = HsSyntax.MkValueName "ptryFrom'"

pconQValName :: HsSyntax.QValName
pconQValName = (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch", HsSyntax.MkValueName "pcon")

pappQValName :: HsSyntax.QValName
pappQValName = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Prelude", HsSyntax.MkValueName "#")

pdataQValName :: HsSyntax.QValName
pdataQValName = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Builtin", HsSyntax.MkValueName "pdata")

peqQValName :: HsSyntax.QValName
peqQValName = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Bool", HsSyntax.MkValueName "#==")

punsafeCoerceQValName :: HsSyntax.QValName
punsafeCoerceQValName = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Unsafe", HsSyntax.MkValueName "punsafeCoerce")

pdataQTyName :: HsSyntax.QTyName
pdataQTyName = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Builtin", HsSyntax.MkTyName "PData")

constQTyName :: HsSyntax.QTyName
constQTyName = (HsSyntax.MkCabalPackageName "base", HsSyntax.MkModuleName "Data.Functor.Const", HsSyntax.MkTyName "Const")

pasDataQTyName :: HsSyntax.QTyName
pasDataQTyName = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Builtin", HsSyntax.MkTyName "PAsData")

ptryFromPAsDataQValName :: HsSyntax.QValName
ptryFromPAsDataQValName = (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch", HsSyntax.MkValueName "ptryFromPAsData")

termQTyName :: HsSyntax.QTyName
termQTyName = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch", HsSyntax.MkTyName "Term")

scopeQTyName :: HsSyntax.QTyName
scopeQTyName = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch", HsSyntax.MkTyName "S")

ptypeQTyName :: HsSyntax.QTyName
ptypeQTyName = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch", HsSyntax.MkTyName "PType")
