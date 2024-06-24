module LambdaBuffers.Codegen.Haskell.Backend.Plutarch.Refs (
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
  showQClassName,
  genericQClassName,
) where

import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as Haskell

plutusTypeQClassName :: Haskell.QClassName
plutusTypeQClassName = (Haskell.MkCabalPackageName "plutarch", Haskell.MkModuleName "Plutarch.Internal.PlutusType", Haskell.MkClassName "PlutusType")

pconMethod :: Haskell.ValueName
pconMethod = Haskell.MkValueName "pcon'"

pmatchMethod :: Haskell.ValueName
pmatchMethod = Haskell.MkValueName "pmatch'"

peqQClassName :: Haskell.QClassName
peqQClassName = (Haskell.MkCabalPackageName "plutarch", Haskell.MkModuleName "Plutarch.Bool", Haskell.MkClassName "PEq")

peqMethod :: Haskell.ValueName
peqMethod = Haskell.MkValueName "#=="

pisDataQClassName :: Haskell.QClassName
pisDataQClassName = (Haskell.MkCabalPackageName "plutarch", Haskell.MkModuleName "Plutarch.Builtin", Haskell.MkClassName "PIsData")

ptryFromQClassName :: Haskell.QClassName
ptryFromQClassName = (Haskell.MkCabalPackageName "plutarch", Haskell.MkModuleName "Plutarch.TryFrom", Haskell.MkClassName "PTryFrom")

ptryFromMethod :: Haskell.ValueName
ptryFromMethod = Haskell.MkValueName "ptryFrom'"

pconQValName :: Haskell.QValName
pconQValName = (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch", Haskell.MkValueName "pcon")

pappQValName :: Haskell.QValName
pappQValName = (Haskell.MkCabalPackageName "plutarch", Haskell.MkModuleName "Plutarch.Prelude", Haskell.MkValueName "#")

pdataQValName :: Haskell.QValName
pdataQValName = (Haskell.MkCabalPackageName "plutarch", Haskell.MkModuleName "Plutarch.Builtin", Haskell.MkValueName "pdata")

peqQValName :: Haskell.QValName
peqQValName = (Haskell.MkCabalPackageName "plutarch", Haskell.MkModuleName "Plutarch.Bool", Haskell.MkValueName "#==")

punsafeCoerceQValName :: Haskell.QValName
punsafeCoerceQValName = (Haskell.MkCabalPackageName "plutarch", Haskell.MkModuleName "Plutarch.Unsafe", Haskell.MkValueName "punsafeCoerce")

pdataQTyName :: Haskell.QTyName
pdataQTyName = (Haskell.MkCabalPackageName "plutarch", Haskell.MkModuleName "Plutarch.Builtin", Haskell.MkTyName "PData")

constQTyName :: Haskell.QTyName
constQTyName = (Haskell.MkCabalPackageName "base", Haskell.MkModuleName "Data.Functor.Const", Haskell.MkTyName "Const")

pasDataQTyName :: Haskell.QTyName
pasDataQTyName = (Haskell.MkCabalPackageName "plutarch", Haskell.MkModuleName "Plutarch.Builtin", Haskell.MkTyName "PAsData")

ptryFromPAsDataQValName :: Haskell.QValName
ptryFromPAsDataQValName = (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch", Haskell.MkValueName "ptryFromPAsData")

termQTyName :: Haskell.QTyName
termQTyName = (Haskell.MkCabalPackageName "plutarch", Haskell.MkModuleName "Plutarch", Haskell.MkTyName "Term")

scopeQTyName :: Haskell.QTyName
scopeQTyName = (Haskell.MkCabalPackageName "plutarch", Haskell.MkModuleName "Plutarch", Haskell.MkTyName "S")

ptypeQTyName :: Haskell.QTyName
ptypeQTyName = (Haskell.MkCabalPackageName "plutarch", Haskell.MkModuleName "Plutarch", Haskell.MkTyName "PType")

showQClassName :: Haskell.QClassName
showQClassName = (Haskell.MkCabalPackageName "plutarch", Haskell.MkModuleName "Plutarch.Show", Haskell.MkClassName "PShow")

genericQClassName :: Haskell.QClassName
genericQClassName = (Haskell.MkCabalPackageName "base", Haskell.MkModuleName "GHC.Generics", Haskell.MkClassName "Generic")
