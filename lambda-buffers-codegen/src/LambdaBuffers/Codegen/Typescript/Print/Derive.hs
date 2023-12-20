module LambdaBuffers.Codegen.Typescript.Print.Derive (tsEqClass, tsIsPlutusDataClass, tsJsonClass, printDeriveEq, printDeriveIsPlutusData, printDeriveJson) where

import Control.Lens qualified as Lens
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl, deriveNeqImpl)
import LambdaBuffers.Codegen.LamVal.Json (deriveFromJsonImpl, deriveToJsonImpl)
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveFromPlutusDataImpl, deriveToPlutusDataImpl)
import LambdaBuffers.Codegen.Typescript.Print.InstanceDef (dict, printInstanceDict)
import LambdaBuffers.Codegen.Typescript.Print.LamVal (Builtin (Builtin, OverloadedBuiltin), printValueE, qualifiedValName)
import LambdaBuffers.Codegen.Typescript.Print.Names (printTsValName)
import LambdaBuffers.Codegen.Typescript.Print.Ty qualified as Typescript.Print.Ty
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (
  Doc,
  align,
  colon,
  comma,
  defaultLayoutOptions,
  langle,
  layoutPretty,
  rangle,
  surround,
  vsep,
  (<+>),
 )
import Proto.Codegen qualified as P

import Data.Text (Text)
import LambdaBuffers.Compiler.LamTy.Types qualified as LamTy.Types
import Prettyprinter.Render.Text qualified as PrettyPrinter.Text

-- Small translation function used to help create instance dictionaries
lamTy2PCTy :: LamTy.Types.Ty -> Maybe PC.Ty
lamTy2PCTy = \case
  LamTy.Types.TyRef ref -> return $ PC.TyRefI ref
  LamTy.Types.TyVar var -> return $ PC.TyVarI var
  LamTy.Types.TyApp _f _args (Just tyApp) -> do
    return $ PC.TyAppI tyApp
  _ -> Nothing

instanceDictIdent :: Ts.QClassName -> PC.Ty -> Text
instanceDictIdent className ty =
  Lens.view (dict . Lens.to (PrettyPrinter.Text.renderStrict . layoutPretty defaultLayoutOptions)) $
    printInstanceDict className ty

lvEqBuiltins :: LV.PrintRead Builtin
lvEqBuiltins = LV.MkPrintRead $ \(tys, refName) ->
  case (refName, tys) of
    ("eq", [ty]) -> do
      ty' <- lamTy2PCTy ty
      return $
        OverloadedBuiltin
          (Ts.primValName $ instanceDictIdent tsEqClass ty')
          -- ( case instanceDictIdent tsEqClass ty' of
          --     -- (TopLevelInstanceDict, dict) -> Ts.normalValName "lbr-prelude" "Prelude" dict
          --     (TopLevelInstanceDict, dict) -> Ts.primValName dict
          --     (ArgumentInstanceDict, dict) -> Ts.primValName dict
          -- )
          0 -- index in the list of substitutions for the type we're overloading on
          ".eq"
    ("true", _) -> Just $ Builtin $ Ts.primValName "true"
    ("false", _) -> Just $ Builtin $ Ts.primValName "false"
    ("and", _) -> Just $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "and"
    ("not", _) -> Just $ Builtin $ Ts.primValName "!"
    _ -> Nothing

eqClassMethodName :: Ts.ValueName
eqClassMethodName = Ts.MkValueName "eq"

neqClassMethodName :: Ts.ValueName
neqClassMethodName = Ts.MkValueName "neq"

tsEqClass :: Ts.QClassName
tsEqClass = (Ts.MkPackageName "lbr-prelude", Ts.MkModuleName "LbrPrelude", Ts.MkClassName "Eq")

printDeriveEq :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Ts.QValName)
printDeriveEq mn iTyDefs mkExportInstanceDeclDoc ty = do
  eqValE <- deriveEqImpl mn iTyDefs ty
  neqValE <- deriveNeqImpl mn iTyDefs ty
  (eqImplDoc, eqImports) <- LV.runPrint lvEqBuiltins (printValueE eqValE)
  (neqImplDoc, neqImports) <- LV.runPrint lvEqBuiltins (printValueE neqValE)
  let eqValueDefDoc =
        align $
          vsep
            [ printTsValName eqClassMethodName <+> colon <+> align eqImplDoc <> comma
            , printTsValName neqClassMethodName <+> colon <+> align neqImplDoc
            ]
  return
    ( mkExportInstanceDeclDoc eqValueDefDoc
    , Set.map (Lens.view qualifiedValName) $ eqImports <> neqImports
    )

lvPlutusDataBuiltins :: LV.PrintRead Builtin
lvPlutusDataBuiltins = LV.MkPrintRead $ \(tys, refName) ->
  case (refName, tys) of
    ("toPlutusData", [ty]) -> do
      ty' <- lamTy2PCTy ty
      return $
        OverloadedBuiltin
          (Ts.primValName $ instanceDictIdent tsEqClass ty')
          -- ( case instanceDictIdent tsEqClass ty' of
          --     (TopLevelInstanceDict, dict) -> Ts.normalValName "cardano-transaction-lib" "Ctl.Internal.ToData" dict
          --     (ArgumentInstanceDict, dict) -> Ts.primValName dict
          -- )
          0
          ".toData"
    ("fromPlutusData", [ty]) -> do
      ty' <- lamTy2PCTy ty
      return $
        OverloadedBuiltin
          (Ts.primValName $ instanceDictIdent tsJsonClass ty')
          -- ( case instanceDictIdent tsEqClass ty' of
          --     (TopLevelInstanceDict, dict) -> Ts.normalValName "cardano-transaction-lib" "Ctl.Internal.fromData" dict
          --     (ArgumentInstanceDict, dict) -> Ts.primValName dict
          -- )
          0
          ".fromData"
    ("casePlutusData", _) -> Just $ Builtin $ Ts.normalValName "lbr-plutus" "LambdaBuffers.Runtime.Plutus" "casePlutusData"
    ("integerData", _) -> Just $ Builtin $ Ts.normalValName "cardano-transaction-lib" "Ctl.Internal.Types.PlutusData" "Integer"
    ("constrData", _) -> Just $ Builtin $ Ts.normalValName "lbr-plutus" "LambdaBuffers.Runtime.Plutus" "pdConstr"
    ("listData", _) -> Just $ Builtin $ Ts.normalValName "cardano-transaction-lib" "Ctl.Internal.Types.PlutusData" "List"
    ("succeedParse", _) -> Just $ Builtin $ Ts.normalValName "maybe" "Data.Maybe" "Just"
    ("failParse", _) -> Just $ Builtin $ Ts.normalValName "maybe" "Data.Maybe" "Nothing"
    ("bindParse", _) -> Just $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "bindMaybe"
    _ -> Nothing

toPlutusDataClassMethodName :: Ts.ValueName
toPlutusDataClassMethodName = Ts.MkValueName "toData"

fromPlutusDataClassMethodName :: Ts.ValueName
fromPlutusDataClassMethodName = Ts.MkValueName "fromData"

tsIsPlutusDataClass :: Ts.QClassName
tsIsPlutusDataClass = (Ts.MkPackageName "lbr-plutus", Ts.MkModuleName "LbPlutus", Ts.MkClassName "IsPlutusData")

printDeriveIsPlutusData :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Ts.QValName)
printDeriveIsPlutusData mn iTyDefs mkExportInstanceDeclDoc ty = do
  toPlutusDataValE <- deriveToPlutusDataImpl mn iTyDefs ty
  fromPlutusDataValE <- deriveFromPlutusDataImpl mn iTyDefs ty

  (toDataImplDoc, toDataImports) <- LV.runPrint lvPlutusDataBuiltins (printValueE toPlutusDataValE)
  (fromDataImplDoc, fromDataImports) <- LV.runPrint lvPlutusDataBuiltins (printValueE fromPlutusDataValE)

  let valueDefDoc =
        align $
          vsep
            [ printTsValName toPlutusDataClassMethodName <+> colon <+> align toDataImplDoc <> comma
            , printTsValName fromPlutusDataClassMethodName <+> colon <+> align fromDataImplDoc <> comma
            ]
  return
    ( mkExportInstanceDeclDoc valueDefDoc
    , Set.map (Lens.view qualifiedValName) $ toDataImports <> fromDataImports
    )

tsJsonClass :: Ts.QClassName
tsJsonClass = (Ts.MkPackageName "lbr-prelude", Ts.MkModuleName "LbrPrelude", Ts.MkClassName "Json")

-- | LambdaBuffers.Codegen.LamVal.Json specification printing
lvJsonBuiltins :: LV.PrintRead Builtin
lvJsonBuiltins = LV.MkPrintRead $ \(tys, refName) ->
  case (refName, tys) of
    ("toJson", [ty]) -> do
      ty' <- lamTy2PCTy ty
      return $
        OverloadedBuiltin
          (Ts.primValName $ instanceDictIdent tsJsonClass ty')
          -- ( case instanceDictIdent tsEqClass ty' of
          --     (TopLevelInstanceDict, dict) -> Ts.normalValName "lbr-prelude" "Prelude" dict
          --     (ArgumentInstanceDict, dict) -> Ts.primValName dict
          -- )
          0
          ".toJson"
    ("fromJson", [ty]) -> do
      ty' <- lamTy2PCTy ty
      return $
        OverloadedBuiltin
          (Ts.primValName (instanceDictIdent tsJsonClass ty'))
          -- ( case instanceDictIdent tsEqClass ty' of
          --     (TopLevelInstanceDict, dict) -> Ts.normalValName "lbr-prelude" "Prelude" dict
          --     (ArgumentInstanceDict, dict) -> Ts.primValName dict
          -- )
          0
          ".fromJson"
    ("jsonObject", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "jsonObject"
    ("jsonConstructor", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "jsonConstructor"
    ("jsonArray", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "jsonArray"
    ("caseJsonConstructor", [ty]) -> do
      ty' <- lamTy2PCTy ty
      return $
        Builtin $
          Ts.normalValName
            "lbr-prelude"
            "LbrPrelude"
            ( PrettyPrinter.Text.renderStrict . layoutPretty defaultLayoutOptions $
                "caseJsonConstructor" <> surround (Typescript.Print.Ty.printTyInner ty') langle rangle
            )
    ("caseJsonArray", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "caseJsonArray"
    ("caseJsonObject", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "caseJsonObject"
    ("jsonField", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "jsonField"
    ("succeedParse", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "succeedParse"
    ("failParse", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "failParse"
    ("bindParse", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "bindParse"
    _ -> Nothing

toJsonClassMethodName :: Ts.ValueName
toJsonClassMethodName = Ts.MkValueName "toJson"

fromJsonClassMethodName :: Ts.ValueName
fromJsonClassMethodName = Ts.MkValueName "fromJson"

printDeriveJson :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Ts.QValName)
printDeriveJson mn iTyDefs mkExportInstanceDeclDoc ty = do
  toJsonValE <- deriveToJsonImpl mn iTyDefs ty
  (toJsonImplDoc, impsA) <- LV.runPrint lvJsonBuiltins (printValueE toJsonValE)
  fromJsonValE <- deriveFromJsonImpl mn iTyDefs ty
  (fromJsonImplDoc, impsB) <- LV.runPrint lvJsonBuiltins (printValueE fromJsonValE)

  let valueDefDoc =
        align $
          vsep
            [ printTsValName toJsonClassMethodName <+> colon <+> align toJsonImplDoc <> comma
            , printTsValName fromJsonClassMethodName <+> colon <+> align fromJsonImplDoc <> comma
            ]

  return
    ( mkExportInstanceDeclDoc valueDefDoc
    , Set.map (Lens.view qualifiedValName) $ impsA <> impsB
    )
