module LambdaBuffers.Codegen.Haskell.Print.Derive (printDeriveEqBase, printDeriveEqPlutusTx, printDeriveToPlutusData, printDeriveFromPlutusData, printDeriveJson, hsClassImplPrinters) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Codegen.Haskell.Print.LamVal (printValueE)
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as H
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl)
import LambdaBuffers.Codegen.LamVal.Json (deriveFromJsonImpl, deriveToJsonImpl)
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveFromPlutusDataImpl, deriveToPlutusDataImpl)
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, equals, vsep, (<+>))
import Proto.Codegen qualified as P

hsClassImplPrinters ::
  Map
    H.QClassName
    ( PC.ModuleName ->
      PC.TyDefs ->
      (Doc ann -> Doc ann) ->
      PC.Ty ->
      Either P.InternalError (Doc ann, Set H.QValName)
    )
hsClassImplPrinters =
  Map.fromList
    [
      ( (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkClassName "Eq")
      , printDeriveEqBase
      )
    ,
      ( (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Eq", H.MkClassName "Eq")
      , printDeriveEqPlutusTx
      )
    ,
      ( (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkClassName "ToData")
      , printDeriveToPlutusData
      )
    ,
      ( (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkClassName "FromData")
      , printDeriveFromPlutusData
      )
    ,
      ( (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkClassName "Json")
      , printDeriveJson
      )
    ]
eqClassMethodName :: H.ValueName
eqClassMethodName = H.MkValueName "=="

lvEqBuiltinsBase :: Map LV.ValueName (H.CabalPackageName, H.ModuleName, H.ValueName)
lvEqBuiltinsBase =
  Map.fromList
    [ ("eq", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "=="))
    , ("and", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "&&"))
    , ("true", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "True"))
    , ("false", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "False"))
    ]

printDeriveEqBase :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set H.QValName)
printDeriveEqBase mn iTyDefs mkInstanceDoc ty = do
  valE <- deriveEqImpl mn iTyDefs ty
  (implDoc, imps) <- LV.runPrint lvEqBuiltinsBase (printValueE valE)
  let instanceDoc = mkInstanceDoc (printValueDef eqClassMethodName implDoc)
  return (instanceDoc, imps)

lvEqBuiltinsPlutusTx :: Map LV.ValueName (H.CabalPackageName, H.ModuleName, H.ValueName)
lvEqBuiltinsPlutusTx =
  Map.fromList
    [ ("eq", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Eq", H.MkValueName "=="))
    , ("and", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "&&"))
    , ("true", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "True"))
    , ("false", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "False"))
    ]

printDeriveEqPlutusTx :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set H.QValName)
printDeriveEqPlutusTx mn iTyDefs mkInstanceDoc ty = do
  valE <- deriveEqImpl mn iTyDefs ty
  (implDoc, imps) <- LV.runPrint lvEqBuiltinsPlutusTx (printValueE valE)
  let instanceDoc = mkInstanceDoc (printValueDef eqClassMethodName implDoc)
  return (instanceDoc, imps)

lvPlutusDataBuiltins :: Map LV.ValueName H.QValName
lvPlutusDataBuiltins =
  Map.fromList
    [ ("toPlutusData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "toBuiltinData"))
    , ("fromPlutusData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "fromBuiltinData"))
    , ("casePlutusData", (H.MkCabalPackageName "lbr-plutus", H.MkModuleName "LambdaBuffers.Runtime.Plutus", H.MkValueName "casePlutusData"))
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
  (implDoc, imps) <- LV.runPrint lvPlutusDataBuiltins (printValueE valE)
  let instanceDoc = mkInstanceDoc (printValueDef toPlutusDataClassMethodName implDoc)
  return
    ( instanceDoc
    , imps
    )

printValueDef :: H.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = H.printHsValName valName <+> equals <+> valDoc

fromPlutusDataClassMethodName :: H.ValueName
fromPlutusDataClassMethodName = H.MkValueName "fromBuiltinData"

builtinDataToDataRef :: H.QValName
builtinDataToDataRef = (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "builtinDataToData")

printDeriveFromPlutusData :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set H.QValName)
printDeriveFromPlutusData mn iTyDefs mkInstanceDoc ty = do
  valE <- deriveFromPlutusDataImpl mn iTyDefs ty
  (implDoc, imps) <- LV.runPrint lvPlutusDataBuiltins (printValueE valE)
  let instanceDoc = mkInstanceDoc (printValueDef fromPlutusDataClassMethodName implDoc)
  return
    ( instanceDoc
    , Set.singleton builtinDataToDataRef <> imps
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
  (toJsonImplDoc, impsA) <- LV.runPrint lvJsonBuiltins (printValueE toJsonValE)
  fromJsonValE <- deriveFromJsonImpl mn iTyDefs ty
  (fromJsonImplDoc, impsB) <- LV.runPrint lvJsonBuiltins (printValueE fromJsonValE)

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
    , impsA <> impsB
    )
