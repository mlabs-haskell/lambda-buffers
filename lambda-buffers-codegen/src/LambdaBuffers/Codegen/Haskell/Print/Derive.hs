module LambdaBuffers.Codegen.Haskell.Print.Derive (printDeriveEq, printDeriveToPlutusData, printDeriveFromPlutusData, printDeriveJson) where

import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Codegen.Haskell.Print.LamVal (printImplementation)
import LambdaBuffers.Codegen.Haskell.Print.Names (printHsValName)
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl)
import LambdaBuffers.Codegen.LamVal.Json (deriveFromJsonImpl, deriveToJsonImpl)
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveFromPlutusDataImpl, deriveToPlutusDataImpl)
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, equals, vsep, (<+>))
import Proto.Codegen qualified as P

lvEqBuiltins :: Map LV.ValueName (H.CabalPackageName, H.ModuleName, H.ValueName)
lvEqBuiltins =
  Map.fromList
    [ ("eq", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "=="))
    , ("and", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "&&"))
    , ("true", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "True"))
    , ("false", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "False"))
    ]

eqClassMethodName :: H.ValueName
eqClassMethodName = H.MkValueName "=="

printDeriveEq :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set H.QValName)
printDeriveEq mn iTyDefs mkInstanceDoc ty = do
  valE <- deriveEqImpl mn iTyDefs ty
  implDoc <- printImplementation lvEqBuiltins valE
  let instanceDoc = mkInstanceDoc (printValueDef eqClassMethodName implDoc)
  return (instanceDoc, Set.fromList . toList $ lvEqBuiltins)

lvPlutusDataBuiltins :: Map LV.ValueName H.QValName
lvPlutusDataBuiltins =
  Map.fromList
    [ ("toPlutusData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "toBuiltinData"))
    , ("fromPlutusData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "fromBuiltinData"))
    , ("casePlutusData", (H.MkCabalPackageName "lb-haskell-runtime", H.MkModuleName "LambdaBuffers.Runtime.PlutusTx", H.MkValueName "casePlutusData"))
    , ("integerData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Builtins", H.MkValueName "mkI"))
    , ("constrData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Builtins", H.MkValueName "mkConstr"))
    , ("listData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Builtins", H.MkValueName "mkList"))
    , ("succeedParse", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "Just"))
    , ("failParse", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "Nothing"))
    , ("bindParse", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName ">>="))
    ]

toPlutusDataClassMethodName :: H.ValueName
toPlutusDataClassMethodName = H.MkValueName "toBuiltinData"

printDeriveToPlutusData :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set H.QValName)
printDeriveToPlutusData mn iTyDefs mkInstanceDoc ty = do
  valE <- deriveToPlutusDataImpl mn iTyDefs ty
  implDoc <- printImplementation lvPlutusDataBuiltins valE
  let instanceDoc = mkInstanceDoc (printValueDef toPlutusDataClassMethodName implDoc)
  return
    ( instanceDoc
    , Set.fromList . toList $ lvPlutusDataBuiltins
    )

printValueDef :: H.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = printHsValName valName <+> equals <+> valDoc

fromPlutusDataClassMethodName :: H.ValueName
fromPlutusDataClassMethodName = H.MkValueName "fromBuiltinData"

builtinDataToDataRef :: H.QValName
builtinDataToDataRef = (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "builtinDataToData")

printDeriveFromPlutusData :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set H.QValName)
printDeriveFromPlutusData mn iTyDefs mkInstanceDoc ty = do
  valE <- deriveFromPlutusDataImpl mn iTyDefs ty
  implDoc <- printImplementation lvPlutusDataBuiltins valE
  let instanceDoc = mkInstanceDoc (printValueDef fromPlutusDataClassMethodName implDoc)
  return
    ( instanceDoc
    , Set.insert builtinDataToDataRef $ Set.fromList . toList $ lvPlutusDataBuiltins
    )

-- | LambdaBuffers.Codegen.LamVal.Json specification printing
lvJsonBuiltins :: Map LV.ValueName H.QValName
lvJsonBuiltins =
  Map.fromList
    [ ("toJson", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "toJson"))
    , ("fromJson", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "fromJson"))
    , ("jsonObject", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "jsonObject"))
    , ("jsonConstructor", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "jsonConstructor"))
    , ("jsonArray", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "jsonArray"))
    , ("caseJsonConstructor", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "caseJsonConstructor"))
    , ("caseJsonArray", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "caseJsonArray"))
    , ("caseJsonObject", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "caseJsonObject"))
    , ("jsonField", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "jsonField"))
    , ("succeedParse", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "return"))
    , ("failParse", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "fail"))
    , ("bindParse", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName ">>="))
    ]

toJsonClassMethodName :: H.ValueName
toJsonClassMethodName = H.MkValueName "toJson"

fromJsonClassMethodName :: H.ValueName
fromJsonClassMethodName = H.MkValueName "fromJson"

printDeriveJson :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set H.QValName)
printDeriveJson mn iTyDefs mkInstanceDoc ty = do
  toJsonValE <- deriveToJsonImpl mn iTyDefs ty
  toJsonImplDoc <- printImplementation lvJsonBuiltins toJsonValE
  fromJsonValE <- deriveFromJsonImpl mn iTyDefs ty
  fromJsonImplDoc <- printImplementation lvJsonBuiltins fromJsonValE

  let instanceDoc =
        mkInstanceDoc
          ( align $
              vsep
                [ printValueDef toJsonClassMethodName toJsonImplDoc
                , printValueDef fromJsonClassMethodName fromJsonImplDoc
                ]
          )
  return
    ( instanceDoc
    , Set.fromList . toList $ lvJsonBuiltins
    )
