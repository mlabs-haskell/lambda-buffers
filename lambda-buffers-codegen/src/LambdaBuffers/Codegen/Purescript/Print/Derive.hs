module LambdaBuffers.Codegen.Purescript.Print.Derive (printDeriveEq, printDeriveToPlutusData, printDeriveFromPlutusData) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl)
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveFromPlutusDataImpl, deriveToPlutusDataImpl)
import LambdaBuffers.Codegen.Purescript.Print.LamVal (printValueE)
import LambdaBuffers.Codegen.Purescript.Print.Names (printPursValName)
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purs
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, equals, (<+>))
import Proto.Codegen qualified as P

lvEqBuiltins :: Map LV.ValueName Purs.QValName
lvEqBuiltins =
  Map.fromList
    [ ("eq", Purs.normalValName "prelude" "Prelude" "==")
    , ("and", Purs.normalValName "prelude" "Prelude" "&&")
    , ("true", Purs.primValName "true")
    , ("false", Purs.primValName "false")
    ]

eqClassMethodName :: Purs.ValueName
eqClassMethodName = Purs.MkValueName "eq"

printDeriveEq :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Purs.QValName)
printDeriveEq mn iTyDefs mkInstanceDoc ty = do
  valE <- deriveEqImpl mn iTyDefs ty
  (implDoc, imports) <- LV.runPrint lvEqBuiltins (printValueE valE)
  let instanceDoc = mkInstanceDoc (printValueDef eqClassMethodName implDoc)
  return (instanceDoc, imports)

lvPlutusDataBuiltins :: Map LV.ValueName Purs.QValName
lvPlutusDataBuiltins =
  Map.fromList
    [ ("toPlutusData", Purs.normalValName "cardano-transaction-lib" "Ctl.Internal.ToData" "toData")
    , ("fromPlutusData", Purs.normalValName "cardano-transaction-lib" "Ctl.Internal.FromData" "fromData")
    , ("casePlutusData", Purs.normalValName "lb-ctl-runtime" "LambdaBuffers.Runtime.PlutusLedgerApi" "casePlutusData")
    , ("integerData", Purs.normalValName "cardano-transaction-lib" "Ctl.Internal.Types.PlutusData" "Integer")
    , ("constrData", Purs.normalValName "lb-ctl-runtime" "LambdaBuffers.Runtime.PlutusLedgerApi" "pdConstr")
    , ("listData", Purs.normalValName "cardano-transaction-lib" "Ctl.Internal.Types.PlutusData" "List")
    , ("succeedParse", Purs.normalValName "maybe" "Data.Maybe" "Just")
    , ("failParse", Purs.normalValName "maybe" "Data.Maybe" "Nothing")
    , ("bindParse", Purs.normalValName "prelude" "Prelude" ">>=")
    ]

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
