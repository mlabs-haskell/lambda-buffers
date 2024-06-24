module LambdaBuffers.Codegen.Purescript.Print.Derive (printDeriveEq, printDeriveToPlutusData, printDeriveFromPlutusData, printDeriveJson) where

import Data.Map qualified as Map
import Data.Set (Set)
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl)
import LambdaBuffers.Codegen.LamVal.Json (deriveFromJsonImpl, deriveToJsonImpl)
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveFromPlutusDataImpl, deriveToPlutusDataImpl)
import LambdaBuffers.Codegen.Purescript.Print.LamVal (printValueE)
import LambdaBuffers.Codegen.Purescript.Print.Names (printPursValName)
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purs
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, equals, vsep, (<+>))
import Proto.Codegen qualified as P

lvEqBuiltins :: LV.Context Purs.QValName ()
lvEqBuiltins =
  LV.Context
    ( \(_ty, refName) ->
        Map.lookup refName $
          Map.fromList
            [ ("eq", Purs.normalValName "prelude" "Prelude" "==")
            , ("and", Purs.normalValName "prelude" "Prelude" "&&")
            , ("true", Purs.primValName "true")
            , ("false", Purs.primValName "false")
            ]
    )
    ()

eqClassMethodName :: Purs.ValueName
eqClassMethodName = Purs.MkValueName "eq"

printDeriveEq :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Purs.QValName)
printDeriveEq mn iTyDefs mkInstanceDoc ty = do
  valE <- deriveEqImpl mn iTyDefs ty
  (implDoc, imports) <- LV.runPrint lvEqBuiltins (printValueE valE)
  let instanceDoc = mkInstanceDoc (printValueDef eqClassMethodName implDoc)
  return (instanceDoc, imports)

lvPlutusDataBuiltins :: LV.Context Purs.QValName ()
lvPlutusDataBuiltins =
  LV.Context
    ( \(_ty, refName) ->
        Map.lookup refName $
          Map.fromList
            [ ("toPlutusData", Purs.normalValName "cardano-transaction-lib" "Ctl.Internal.ToData" "toData")
            , ("fromPlutusData", Purs.normalValName "cardano-transaction-lib" "Ctl.Internal.FromData" "fromData")
            , ("casePlutusData", Purs.normalValName "lbr-plutus" "LambdaBuffers.Runtime.Plutus" "casePlutusData")
            , ("integerData", Purs.normalValName "cardano-transaction-lib" "Ctl.Internal.Types.PlutusData" "Integer")
            , ("constrData", Purs.normalValName "lbr-plutus" "LambdaBuffers.Runtime.Plutus" "pdConstr")
            , ("listData", Purs.normalValName "cardano-transaction-lib" "Ctl.Internal.Types.PlutusData" "List")
            , ("succeedParse", Purs.normalValName "maybe" "Data.Maybe" "Just")
            , ("failParse", Purs.normalValName "maybe" "Data.Maybe" "Nothing")
            , ("bindParse", Purs.normalValName "prelude" "Prelude" ">>=")
            ]
    )
    ()

toPlutusDataClassMethodName :: Purs.ValueName
toPlutusDataClassMethodName = Purs.MkValueName "toData"

fromPlutusDataClassMethodName :: Purs.ValueName
fromPlutusDataClassMethodName = Purs.MkValueName "fromData"

printDeriveToPlutusData :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Purs.QValName)
printDeriveToPlutusData mn iTyDefs mkInstanceDoc ty = do
  valE <- deriveToPlutusDataImpl mn iTyDefs ty
  (implDoc, imports) <- LV.runPrint lvPlutusDataBuiltins (printValueE valE)
  let instanceDoc = mkInstanceDoc (printValueDef toPlutusDataClassMethodName implDoc)
  return
    ( instanceDoc
    , imports
    )

printDeriveFromPlutusData :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Purs.QValName)
printDeriveFromPlutusData mn iTyDefs mkInstanceDoc ty = do
  valE <- deriveFromPlutusDataImpl mn iTyDefs ty
  (implDoc, imports) <- LV.runPrint lvPlutusDataBuiltins (printValueE valE)
  let instanceDoc = mkInstanceDoc (printValueDef fromPlutusDataClassMethodName implDoc)
  return
    ( instanceDoc
    , imports
    )

printValueDef :: Purs.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = printPursValName valName <+> equals <+> valDoc

-- | LambdaBuffers.Codegen.LamVal.Json specification printing
lvJsonBuiltins :: LV.Context Purs.QValName ()
lvJsonBuiltins =
  LV.Context
    ( \(_ty, refName) ->
        Map.lookup refName $
          Map.fromList
            [ ("toJson", Purs.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "toJson")
            , ("fromJson", Purs.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "fromJson")
            , ("jsonObject", Purs.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "jsonObject")
            , ("jsonConstructor", Purs.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "jsonConstructor")
            , ("jsonArray", Purs.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "jsonArray")
            , ("caseJsonConstructor", Purs.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "caseJsonConstructor")
            , ("caseJsonArray", Purs.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "caseJsonArray")
            , ("caseJsonObject", Purs.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "caseJsonObject")
            , ("jsonField", Purs.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "jsonField")
            , ("succeedParse", Purs.normalValName "either" "Data.Either" "Right")
            , ("failParse", Purs.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "fail")
            , ("bindParse", Purs.normalValName "prelude" "Prelude" ">>=")
            ]
    )
    ()

toJsonClassMethodName :: Purs.ValueName
toJsonClassMethodName = Purs.MkValueName "toJson"

fromJsonClassMethodName :: Purs.ValueName
fromJsonClassMethodName = Purs.MkValueName "fromJson"

printDeriveJson :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Purs.QValName)
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
