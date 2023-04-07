module LambdaBuffers.Codegen.Purescript.Print.Derive (printDeriveEq, printDeriveToPlutusData, printDeriveFromPlutusData) where

import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl)
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveFromPlutusDataImpl, deriveToPlutusDataImpl)
import LambdaBuffers.Codegen.Purescript.Print.LamVal (printImplementation)
import LambdaBuffers.Codegen.Purescript.Print.Names (printPursValName)
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purs
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, equals, (<+>))

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

-- TODO: Handle errors properly.
printDeriveEq :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either Text (Doc ann, Set Purs.QValName)
printDeriveEq mn iTyDefs mkInstanceDoc ty =
  case deriveEqImpl mn iTyDefs ty of
    Left err -> Left $ Text.pack err
    Right valE ->
      case printImplementation lvEqBuiltins valE of
        Left err -> Left $ Text.pack $ show err
        Right implDoc ->
          let instanceDoc = mkInstanceDoc (printValueDef eqClassMethodName implDoc)
           in Right (instanceDoc, Set.fromList . toList $ lvEqBuiltins)

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

-- TODO: Handle errors properly.
printDeriveToPlutusData :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either Text (Doc ann, Set Purs.QValName)
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

printDeriveFromPlutusData :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either Text (Doc ann, Set Purs.QValName)
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
                , Set.fromList . toList $ lvPlutusDataBuiltins
                )

printValueDef :: Purs.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = printPursValName valName <+> equals <+> valDoc
