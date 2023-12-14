module LambdaBuffers.Codegen.Rust.Print.Derive (printDeriveEqBase, printDeriveIsPlutusData, printDeriveJson, rsTraitImplPrinters) where

import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl)
import LambdaBuffers.Codegen.LamVal.Json (deriveFromJsonImpl, deriveToJsonImpl)
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveFromPlutusDataImpl, deriveToPlutusDataImpl)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Rust.Print (MonadPrint)
import LambdaBuffers.Codegen.Rust.Print.LamVal (printInstance)
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, braces, colon, comma, encloseSep, hcat, indent, lparen, parens, rparen, space, vsep, (<+>))
import Proto.Codegen qualified as P
import Proto.Codegen_Fields qualified as P

rsTraitImplPrinters ::
  MonadPrint m =>
  Map
    R.QTraitName
    ( PC.ModuleName ->
      R.PkgMap ->
      PC.TyDefs ->
      (Doc ann -> Doc ann) ->
      PC.Ty ->
      m (Doc ann)
    )
rsTraitImplPrinters =
  Map.fromList
    [
      ( R.qLibRef R.MkTraitName "std" "cmp" "PartialEq"
      , printDerivePartialEqBase
      )
    ,
      ( R.qLibRef R.MkTraitName "std" "cmp" "Eq"
      , printDeriveEqBase
      )
    ,
      ( R.qLibRef R.MkTraitName "plutus-ledger-api" "plutus_data" "IsPlutusData"
      , printDeriveIsPlutusData
      )
    ,
      ( R.qLibRef R.MkTraitName "lbr-prelude" "json" "Json"
      , printDeriveJson
      )
    ]
eqTraitMethodName :: R.ValueName
eqTraitMethodName = R.MkValueName "eq"

eqTraitMethodArgs :: [(R.ValueName, R.QTyName)]
eqTraitMethodArgs = [(R.MkValueName "self", R.qBuiltin R.MkTyName "Self"), (R.MkValueName "other", R.qBuiltin R.MkTyName "Self")]

eqTraitMethodReturns :: R.QTyName
eqTraitMethodReturns = R.qBuiltin R.MkTyName "bool"

lvEqBuiltinsBase :: LV.PrintRead R.QValName
lvEqBuiltinsBase = LV.MkPrintRead $ \(_ty, refName) ->
  Map.lookup refName $
    Map.fromList
      [ ("eq", R.qLibRef R.MkValueName "lbr-prelude" "lamval" "eq")
      , ("and", R.qLibRef R.MkValueName "lbr-prelude" "lamval" "and")
      , ("true", R.qBuiltin R.MkValueName "true")
      , ("false", R.qBuiltin R.MkValueName "false")
      , ("PhantomData", R.qLibRef R.MkValueName "std" "marker" "PhantomData")
      ]

printDerivePartialEqBase :: MonadPrint m => PC.ModuleName -> R.PkgMap -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> m (Doc ann)
printDerivePartialEqBase mn pkgs iTyDefs mkInstance ty = do
  case deriveEqImpl mn iTyDefs ty of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Deriving Prelude.Eq LamVal implementation from a type failed with: " <> err ^. P.msg)
    Right valE -> do
      case LV.runPrint lvEqBuiltinsBase (printInstance pkgs [R.qBuiltin R.MkTyName "Self", R.qBuiltin R.MkTyName "Self"] iTyDefs valE) of
        Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Interpreting LamVal into Rust failed with: " <> err ^. P.msg)
        Right (implDoc, imps) -> do
          for_ imps Print.importValue
          return $
            mkInstance $
              printTraitMethod eqTraitMethodName eqTraitMethodArgs eqTraitMethodReturns implDoc

printDeriveEqBase :: MonadPrint m => PC.ModuleName -> R.PkgMap -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> m (Doc ann)
printDeriveEqBase _ _ _ mkInstance _ = return $ mkInstance mempty

lvPlutusDataBuiltins :: LV.PrintRead R.QValName
lvPlutusDataBuiltins = LV.MkPrintRead $ \(_ty, refName) ->
  Map.lookup refName $
    Map.fromList
      [ ("toPlutusData", R.qLibRef R.MkValueName "plutus-ledger-api" "plutus_data::IsPlutusData" "to_plutus_data")
      , ("fromPlutusData", R.qLibRef R.MkValueName "plutus-ledger-api" "plutus_data::IsPlutusData" "from_plutus_data")
      , ("casePlutusData", R.qLibRef R.MkValueName "plutus-ledger-api" "lamval" "case_plutus_data")
      , ("integerData", R.qLibRef R.MkValueName "plutus-ledger-api" "plutus_data" "PlutusData::integer")
      , ("constrData", R.qLibRef R.MkValueName "plutus-ledger-api" "lamval" "constr")
      , ("listData", R.qLibRef R.MkValueName "plutus-ledger-api" "plutus_data" "PlutusData::list")
      , ("succeedParse", R.qLibRef R.MkValueName "std" "result" "Result::Ok")
      , ("failParse", R.qLibRef R.MkValueName "plutus-ledger-api" "lamval" "fail_parse()")
      , ("bindParse", R.qLibRef R.MkValueName "plutus-ledger-api" "lamval" "bind_parse")
      ]

toPlutusDataTraitMethodName :: R.ValueName
toPlutusDataTraitMethodName = R.MkValueName "to_plutus_data"

toPlutusDataTraitMethodArgs :: [(R.ValueName, R.QTyName)]
toPlutusDataTraitMethodArgs = [(R.MkValueName "self", R.qBuiltin R.MkTyName "Self")]

toPlutusDataTraitMethodReturns :: R.QTyName
toPlutusDataTraitMethodReturns =
  R.qLibRef R.MkTyName "plutus-ledger-api" "plutus_data" "PlutusData"

fromPlutusDataTraitMethodName :: R.ValueName
fromPlutusDataTraitMethodName = R.MkValueName "from_plutus_data"

fromPlutusDataTraitMethodArgs :: [(R.ValueName, R.QTyName)]
fromPlutusDataTraitMethodArgs =
  [
    ( R.MkValueName "plutus_data"
    , R.qLibRef
        R.MkTyName
        "plutus-ledger-api"
        "plutus_data"
        "PlutusData"
    )
  ]

fromPlutusDataTraitMethodReturns :: R.QTyName
fromPlutusDataTraitMethodReturns =
  R.qLibRef
    R.MkTyName
    "std"
    "result"
    "Result<Self, plutus_ledger_api::plutus_data::PlutusDataError>"

printDeriveIsPlutusData :: MonadPrint m => PC.ModuleName -> R.PkgMap -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> m (Doc ann)
printDeriveIsPlutusData mn pkgs iTyDefs mkInstanceDoc ty = do
  case printDeriveIsPlutusData' mn pkgs iTyDefs mkInstanceDoc ty of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Deriving Prelude.IsPlutusData LamVal implementation from a type failed with: " <> err ^. P.msg)
    Right (plutusDataInstDefDoc, imps) -> do
      for_ imps Print.importValue
      return plutusDataInstDefDoc

printDeriveIsPlutusData' :: PC.ModuleName -> R.PkgMap -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set R.QValName)
printDeriveIsPlutusData' mn pkgs iTyDefs mkInstanceDoc ty = do
  let extraDeps = Set.singleton (R.qLibRef R.MkValueName "serde_json" "" "Value")
  toPlutusDataValE <- deriveToPlutusDataImpl mn iTyDefs ty
  (toPlutusDataImplDoc, impsA) <- LV.runPrint lvPlutusDataBuiltins (printInstance pkgs [R.qBuiltin R.MkTyName "Self"] iTyDefs toPlutusDataValE)
  fromPlutusDataValE <- deriveFromPlutusDataImpl mn iTyDefs ty
  (fromPlutusDataImplDoc, impsB) <- LV.runPrint lvPlutusDataBuiltins (printInstance pkgs [R.qLibRef R.MkTyName "plutus-ledger-api" "plutus_data" "PlutusData"] iTyDefs fromPlutusDataValE)

  let instanceDoc =
        mkInstanceDoc
          ( align $
              vsep
                [ printTraitMethod
                    toPlutusDataTraitMethodName
                    toPlutusDataTraitMethodArgs
                    toPlutusDataTraitMethodReturns
                    toPlutusDataImplDoc
                , printTraitMethod
                    fromPlutusDataTraitMethodName
                    fromPlutusDataTraitMethodArgs
                    fromPlutusDataTraitMethodReturns
                    fromPlutusDataImplDoc
                ]
          )
  return
    ( instanceDoc
    , impsA <> impsB <> extraDeps
    )

-- | LambdaBuffers.Codegen.LamVal.Json specification printing
lvJsonBuiltins :: LV.PrintRead R.QValName
lvJsonBuiltins = LV.MkPrintRead $ \(_ty, refName) ->
  Map.lookup refName $
    Map.fromList
      [ ("toJson", R.qLibRef R.MkValueName "lbr-prelude" "json::Json" "to_json")
      , ("fromJson", R.qLibRef R.MkValueName "lbr-prelude" "json::Json" "from_json")
      , ("jsonObject", R.qLibRef R.MkValueName "lbr-prelude" "json::lamval" "json_object")
      , ("jsonConstructor", R.qLibRef R.MkValueName "lbr-prelude" "json::lamval" "json_constructor")
      , ("jsonArray", R.qLibRef R.MkValueName "lbr-prelude" "json::lamval" "json_array")
      , ("caseJsonConstructor", R.qLibRef R.MkValueName "lbr-prelude" "json::lamval" "case_json_constructor")
      , ("caseJsonArray", R.qLibRef R.MkValueName "lbr-prelude" "json::lamval" "case_json_array")
      , ("caseJsonObject", R.qLibRef R.MkValueName "lbr-prelude" "json::lamval" "case_json_object")
      , ("jsonField", R.qLibRef R.MkValueName "lbr-prelude" "json::lamval" "json_field")
      , ("succeedParse", R.qLibRef R.MkValueName "std" "result" "Result::Ok")
      , ("failParse", R.qLibRef R.MkValueName "lbr-prelude" "json::lamval" "fail_parse")
      , ("bindParse", R.qLibRef R.MkValueName "lbr-prelude" "json::lamval" "bind_parse")
      ]

toJsonTraitMethodName :: R.ValueName
toJsonTraitMethodName = R.MkValueName "to_json"

toJsonTraitMethodArgs :: [(R.ValueName, R.QTyName)]
toJsonTraitMethodArgs = [(R.MkValueName "self", R.qBuiltin R.MkTyName "Self")]

toJsonTraitMethodReturns :: R.QTyName
toJsonTraitMethodReturns =
  R.qLibRef R.MkTyName "serde_json" "" "Value"

fromJsonTraitMethodName :: R.ValueName
fromJsonTraitMethodName = R.MkValueName "from_json"

fromJsonTraitMethodArgs :: [(R.ValueName, R.QTyName)]
fromJsonTraitMethodArgs = [(R.MkValueName "value", R.qLibRef R.MkTyName "serde_json" "" "Value")]

fromJsonTraitMethodReturns :: R.QTyName
fromJsonTraitMethodReturns =
  R.qLibRef R.MkTyName "std" "result" "Result<Self, lbr_prelude::error::Error>" -- TODO(szg251): This is a hack

printDeriveJson :: MonadPrint m => PC.ModuleName -> R.PkgMap -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> m (Doc ann)
printDeriveJson mn pkgs iTyDefs mkInstanceDoc ty = do
  case printDeriveJson' mn pkgs iTyDefs mkInstanceDoc ty of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Deriving Prelude.Json LamVal implementation from a type failed with: " <> err ^. P.msg)
    Right (jsonInstDefDoc, imps) -> do
      for_ imps Print.importValue
      return jsonInstDefDoc

printDeriveJson' :: PC.ModuleName -> R.PkgMap -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set R.QValName)
printDeriveJson' mn pkgs iTyDefs mkInstanceDoc ty = do
  toJsonValE <- deriveToJsonImpl mn iTyDefs ty
  (toJsonImplDoc, impsA) <- LV.runPrint lvJsonBuiltins (printInstance pkgs [R.qBuiltin R.MkTyName "Self"] iTyDefs toJsonValE)
  fromJsonValE <- deriveFromJsonImpl mn iTyDefs ty
  (fromJsonImplDoc, impsB) <- LV.runPrint lvJsonBuiltins (printInstance pkgs [] iTyDefs fromJsonValE)

  let instanceDoc =
        mkInstanceDoc
          ( align $
              vsep
                [ printTraitMethod
                    toJsonTraitMethodName
                    toJsonTraitMethodArgs
                    toJsonTraitMethodReturns
                    toJsonImplDoc
                , printTraitMethod
                    fromJsonTraitMethodName
                    fromJsonTraitMethodArgs
                    fromJsonTraitMethodReturns
                    fromJsonImplDoc
                ]
          )
  return
    ( instanceDoc
    , impsA <> impsB <> Set.singleton (R.qLibRef R.MkValueName "serde_json" "" "Value")
    )

{- | Print a trait method implementation
 To allow using a `LamE` in the body of the method, capturing the arguments,
 we're adding these

 ```rust
 fn <fnName>(<arg1>: <tyName1, <arg2>: <tyName2>) -> <returns> {
  <implBody>(&<arg1>)(&<arg2>)
 }
 ```
-}
printTraitMethod ::
  R.ValueName -> [(R.ValueName, R.QTyName)] -> R.Qualified R.TyName -> Doc ann -> Doc ann
printTraitMethod fnName args returns implDoc =
  let argsWithTypes =
        encloseSep lparen rparen comma $ (\(arg, ty) -> R.printRsValName arg <> colon <+> "&'a " <> R.printRsQTyName ty) <$> args
      argsLst = hcat $ parens . R.printRsValName . fst <$> args
   in indent 4 $
        "fn"
          <+> R.printRsValName fnName
            <> "<'a>"
            <> argsWithTypes
          <+> "->"
          <+> R.printRsQTyName returns
          <+> braces (space <> implDoc <> argsLst)
