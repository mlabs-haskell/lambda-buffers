module LambdaBuffers.Codegen.Haskell.Print.Derive (printDeriveEq, printDeriveToPlutusData, printDeriveFromPlutusData) where

import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Print.LamVal (printImplementation)
import LambdaBuffers.Codegen.Haskell.Print.Names (printHsValName)
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl)
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveFromPlutusDataImpl, deriveToPlutusDataImpl)
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, equals, (<+>))

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

-- TODO: Handle errors properly.
printDeriveEq :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either Text (Doc ann, Set H.QValName)
printDeriveEq mn iTyDefs mkInstanceDoc ty =
  case deriveEqImpl mn iTyDefs ty of
    Left err -> Left $ Text.pack err
    Right valE ->
      case printImplementation lvEqBuiltins valE of
        Left err -> Left $ Text.pack $ show err
        Right implDoc ->
          let instanceDoc = mkInstanceDoc (printValueDef eqClassMethodName implDoc)
           in Right (instanceDoc, Set.fromList . toList $ lvEqBuiltins)

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

-- TODO: Handle errors properly.
printDeriveToPlutusData :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either Text (Doc ann, Set H.QValName)
printDeriveToPlutusData mn iTyDefs mkInstanceDoc ty =
  case deriveToPlutusDataImpl mn iTyDefs ty of
    Left err -> Left $ Text.pack err
    Right valE ->
      case printImplementation lvPlutusDataBuiltins valE of
        Left err -> Left $ Text.pack $ show err
        Right implDoc ->
          let instanceDoc = mkInstanceDoc (printValueDef toPlutusDataClassMethodName implDoc)
           in Right
                ( instanceDoc
                , Set.fromList . toList $ lvPlutusDataBuiltins
                )

printValueDef :: H.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = printHsValName valName <+> equals <+> valDoc

fromPlutusDataClassMethodName :: H.ValueName
fromPlutusDataClassMethodName = H.MkValueName "fromBuiltinData"

builtinDataToDataRef :: H.QValName
builtinDataToDataRef = (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "builtinDataToData")

printDeriveFromPlutusData :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either Text (Doc ann, Set H.QValName)
printDeriveFromPlutusData mn iTyDefs mkInstanceDoc ty =
  case deriveFromPlutusDataImpl mn iTyDefs ty of
    Left err -> Left $ Text.pack err
    Right valE ->
      case printImplementation lvPlutusDataBuiltins valE of
        Left err -> Left $ Text.pack $ show err
        Right implDoc ->
          let instanceDoc = mkInstanceDoc (printValueDef fromPlutusDataClassMethodName implDoc)
           in Right
                ( instanceDoc
                , Set.insert builtinDataToDataRef $ Set.fromList . toList $ lvPlutusDataBuiltins
                )
