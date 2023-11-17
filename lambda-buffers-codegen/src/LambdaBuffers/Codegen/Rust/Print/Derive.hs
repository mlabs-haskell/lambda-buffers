module LambdaBuffers.Codegen.Rust.Print.Derive (printDeriveEqBase, printDeriveIsPlutusData, printDeriveJson, rsClassImplPrinters) where

import Control.Lens ((^.))
import Data.Bifunctor (second)
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
import LambdaBuffers.Codegen.Rust.Print.LamVal (printFunc, printValueE)
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, braces, equals, hardline, indent, space, vsep, (<+>))
import Proto.Codegen qualified as P
import Proto.Codegen_Fields qualified as P

rsClassImplPrinters ::
  MonadPrint m =>
  Map
    R.QTraitName
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
eqClassMethodName = R.MkValueName "eq"

lvEqBuiltinsBase :: Map LV.ValueName (R.CrateName, R.ModuleName, R.ValueName)
lvEqBuiltinsBase =
  Map.fromList
    [ ("eq", (R.MkCrateName "std", R.MkModuleName "cmp", R.MkValueName "eq"))
    , ("and", (R.MkCrateName "std", R.MkModuleName "ops", R.MkValueName "bitand"))
    , ("true", (R.MkCrateName "std", R.MkModuleName "primitive", R.MkValueName "true"))
    , ("false", (R.MkCrateName "std", R.MkModuleName "primitive", R.MkValueName "false"))
    ]

printDeriveEqBase :: MonadPrint m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> m (Doc ann)
printDeriveEqBase mn iTyDefs mkInstanceDoc ty = do
  case deriveEqImpl mn iTyDefs ty of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Deriving Prelude.Eq LamVal implementation from a type failed with: " <> err ^. P.msg)
    Right valE -> do
      case LV.runPrint lvEqBuiltinsBase (printFunc eqClassMethodName valE) of
        Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Interpreting LamVal into Rust failed with: " <> err ^. P.msg)
        Right (implDoc, imps) -> do
          for_ imps Print.importValue
          return implDoc

lvPlutusDataBuiltins :: Map LV.ValueName R.QValName
lvPlutusDataBuiltins =
  Map.fromList
    [ ("toPlutusData", (R.MkCrateName "plutus-ledger-api", R.MkModuleName "plutus-data", R.MkValueName "to_plutus_data"))
    , ("fromPlutusData", (R.MkCrateName "plutus-ledger-api", R.MkModuleName "plutus-data", R.MkValueName "from_plutus_data"))
    , ("casePlutusData", (R.MkCrateName "plutus-ledger-api", R.MkModuleName "LambdaBuffers.Runtime.Plutus", R.MkValueName "casePlutusData"))
    , ("integerData", (R.MkCrateName "plutus-ledger-api", R.MkModuleName "plutus-data", R.MkValueName "mkI"))
    , ("constrData", (R.MkCrateName "plutus-ledger-api", R.MkModuleName "plutus-data", R.MkValueName "mkConstr"))
    , ("listData", (R.MkCrateName "plutus-tx", R.MkModuleName "PlutusTx.Builtins", R.MkValueName "mkList"))
    ]

toPlutusDataClassMethodName :: R.ValueName
toPlutusDataClassMethodName = R.MkValueName "to_plutus_data"

printDeriveIsPlutusData :: MonadPrint m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> m (Doc ann)
printDeriveIsPlutusData mn iTyDefs mkInstanceDoc ty = do
  case deriveToPlutusDataImpl mn iTyDefs ty of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Deriving Plutus.V1.PlutusData LamVal implementation from a type failed with: " <> err ^. P.msg)
    Right valE -> do
      case LV.runPrint lvPlutusDataBuiltins (printFunc toPlutusDataClassMethodName valE) of
        Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Interpreting LamVal into Rust failed with: " <> err ^. P.msg)
        Right (implDoc, imps) -> do
          for_ imps Print.importValue
          return implDoc

printValueDef :: R.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc =
  indent 4 $ "fn" <+> R.printRsValName valName <+> equals <+> braces (space <> valDoc)

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
toJsonClassMethodName = R.MkValueName "to_json"

fromJsonClassMethodName :: R.ValueName
fromJsonClassMethodName = R.MkValueName "from_json"

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
  (toJsonImplDoc, impsA) <- LV.runPrint lvJsonBuiltins (printFunc toJsonClassMethodName toJsonValE)
  fromJsonValE <- deriveFromJsonImpl mn iTyDefs ty
  (fromJsonImplDoc, impsB) <- LV.runPrint lvJsonBuiltins (printFunc fromJsonClassMethodName fromJsonValE)

  let instanceDoc =
        mkInstanceDoc
          ( align $
              vsep
                [ toJsonImplDoc
                , fromJsonImplDoc
                ]
          )
  return
    ( instanceDoc
    , impsA <> impsB
    )
