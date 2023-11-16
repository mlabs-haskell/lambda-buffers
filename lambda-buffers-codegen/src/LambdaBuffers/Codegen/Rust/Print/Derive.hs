module LambdaBuffers.Codegen.Rust.Print.Derive (printDeriveEqBase, printDeriveIsPlutusData, printDeriveJson, rsClassImplPrinters) where

import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl)
import LambdaBuffers.Codegen.LamVal.Json (deriveFromJsonImpl, deriveToJsonImpl)
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveToPlutusDataImpl)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Rust.Print (MonadPrint)
import LambdaBuffers.Codegen.Rust.Print.LamVal (printValueE)
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, equals, hardline, vsep, (<+>))
import Proto.Codegen qualified as P
import Proto.Codegen_Fields qualified as P

rsClassImplPrinters ::
  MonadPrint m =>
  Map
    R.QClassName
    ( PC.ModuleName ->
      PC.TyDefs ->
      (Doc ann -> Doc ann) ->
      PC.Ty ->
      m (Doc ann)
    )
rsClassImplPrinters =
  Map.fromList
    [
      ( (R.MkCrateName "std", R.MkModuleName "cmp", R.MkClassName "Eq")
      , printDeriveEqBase
      )
    ,
      ( (R.MkCrateName "plutus-ledger-api", R.MkModuleName "plutus-data", R.MkClassName "IsPlutusData")
      , printDeriveIsPlutusData
      )
    ,
      ( (R.MkCrateName "lbr-prelude", R.MkModuleName "json", R.MkClassName "Json")
      , printDeriveJson
      )
    ]
eqClassMethodName :: R.ValueName
eqClassMethodName = R.MkValueName "=="

lvEqBuiltinsBase :: Map LV.ValueName (R.CrateName, R.ModuleName, R.ValueName)
lvEqBuiltinsBase =
  Map.fromList
    [ ("eq", (R.MkCrateName "base", R.MkModuleName "Prelude", R.MkValueName "=="))
    , ("and", (R.MkCrateName "base", R.MkModuleName "Prelude", R.MkValueName "&&"))
    , ("true", (R.MkCrateName "base", R.MkModuleName "Prelude", R.MkValueName "True"))
    , ("false", (R.MkCrateName "base", R.MkModuleName "Prelude", R.MkValueName "False"))
    ]

printDeriveEqBase :: MonadPrint m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> m (Doc ann)
printDeriveEqBase mn iTyDefs mkInstanceDoc ty = do
  case deriveEqImpl mn iTyDefs ty of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Deriving Prelude.Eq LamVal implementation from a type failed with: " <> err ^. P.msg)
    Right valE -> do
      case LV.runPrint lvEqBuiltinsBase (printValueE valE) of
        Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Interpreting LamVal into Rust failed with: " <> err ^. P.msg)
        Right (implDoc, imps) -> do
          let instanceDoc = mkInstanceDoc (printValueDef eqClassMethodName implDoc)
          for_ imps Print.importValue
          return instanceDoc

lvPlutusDataBuiltins :: Map LV.ValueName R.QValName
lvPlutusDataBuiltins =
  Map.fromList
    [ ("toPlutusData", (R.MkCrateName "plutus-ledger-api", R.MkModuleName "plutus-data", R.MkValueName "toBuiltinData"))
    , ("fromPlutusData", (R.MkCrateName "plutus-ledger-api", R.MkModuleName "plutus-data", R.MkValueName "fromBuiltinData"))
    , ("casePlutusData", (R.MkCrateName "plutus-ledger-api", R.MkModuleName "LambdaBuffers.Runtime.Plutus", R.MkValueName "casePlutusData"))
    , ("integerData", (R.MkCrateName "plutus-ledger-api", R.MkModuleName "plutus-data", R.MkValueName "mkI"))
    , ("constrData", (R.MkCrateName "plutus-ledger-api", R.MkModuleName "plutus-data", R.MkValueName "mkConstr"))
    , ("listData", (R.MkCrateName "plutus-tx", R.MkModuleName "PlutusTx.Builtins", R.MkValueName "mkList"))
    ]

toPlutusDataClassMethodName :: R.ValueName
toPlutusDataClassMethodName = R.MkValueName "toBuiltinData"

printDeriveIsPlutusData :: MonadPrint m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> m (Doc ann)
printDeriveIsPlutusData mn iTyDefs mkInstanceDoc ty = do
  case deriveToPlutusDataImpl mn iTyDefs ty of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Deriving Plutus.V1.PlutusData LamVal implementation from a type failed with: " <> err ^. P.msg)
    Right valE -> do
      case LV.runPrint lvPlutusDataBuiltins (printValueE valE) of
        Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Interpreting LamVal into Rust failed with: " <> err ^. P.msg)
        Right (implDoc, imps) -> do
          let instanceDoc = mkInstanceDoc (align $ R.printRsValName toPlutusDataClassMethodName <> hardline <> printValueDef toPlutusDataClassMethodName implDoc)
          for_ imps Print.importValue
          return instanceDoc

printValueDef :: R.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = R.printRsValName valName <+> equals <+> valDoc

-- | LambdaBuffers.Codegen.LamVal.Json specification printing
lvJsonBuiltins :: Map LV.ValueName R.QValName
lvJsonBuiltins =
  Map.fromList
    [ ("toJson", (R.MkCrateName "lbr-prelude", R.MkModuleName "json", R.MkValueName "to_json"))
    , ("fromJson", (R.MkCrateName "lbr-prelude", R.MkModuleName "json", R.MkValueName "from_json"))
    , ("jsonObject", (R.MkCrateName "lbr-prelude", R.MkModuleName "json", R.MkValueName "jsonObject"))
    , ("jsonConstructor", (R.MkCrateName "lbr-prelude", R.MkModuleName "json", R.MkValueName "sum_constructor"))
    , ("jsonArray", (R.MkCrateName "lbr-prelude", R.MkModuleName "json", R.MkValueName "jsonArray"))
    , ("caseJsonConstructor", (R.MkCrateName "lbr-prelude", R.MkModuleName "json", R.MkValueName "caseJsonConstructor"))
    , ("caseJsonArray", (R.MkCrateName "lbr-prelude", R.MkModuleName "json", R.MkValueName "caseJsonArray"))
    , ("caseJsonObject", (R.MkCrateName "lbr-prelude", R.MkModuleName "json", R.MkValueName "caseJsonObject"))
    , ("jsonField", (R.MkCrateName "lbr-prelude", R.MkModuleName "json", R.MkValueName "jsonField"))
    , ("succeedParse", (R.MkCrateName "base", R.MkModuleName "Prelude", R.MkValueName "return"))
    , ("failParse", (R.MkCrateName "base", R.MkModuleName "Prelude", R.MkValueName "fail"))
    , ("bindParse", (R.MkCrateName "base", R.MkModuleName "Prelude", R.MkValueName ">>="))
    ]

toJsonClassMethodName :: R.ValueName
toJsonClassMethodName = R.MkValueName "toJson"

fromJsonClassMethodName :: R.ValueName
fromJsonClassMethodName = R.MkValueName "fromJson"

printDeriveJson :: MonadPrint m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> m (Doc ann)
printDeriveJson mn iTyDefs mkInstanceDoc ty = do
  case printDeriveJson' mn iTyDefs mkInstanceDoc ty of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Deriving Prelude.Json LamVal implementation from a type failed with: " <> err ^. P.msg)
    Right (jsonInstDefDoc, imps) -> do
      for_ imps Print.importValue
      return jsonInstDefDoc

printDeriveJson' :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set R.QValName)
printDeriveJson' mn iTyDefs mkInstanceDoc ty = do
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
