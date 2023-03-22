module LambdaBuffers.Codegen.Purescript.Print.Derive (printDeriveEq, printDeriveToPlutusData) where

import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl)
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveToPlutusDataImpl)
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
    , ("integerData", Purs.normalValName "cardano-transaction-lib" "Ctl.Internal.Types.PlutusData" "Integer")
    , ("constrData", Purs.normalValName "cardano-transaction-lib" "Ctl.Internal.Types.PlutusData" "Constr")
    , ("listData", Purs.normalValName "cardano-transaction-lib" "Ctl.Internal.Types.PlutusData" "List")
    ]

toPlutusDataClassMethodName :: Purs.ValueName
toPlutusDataClassMethodName = Purs.MkValueName "toData"

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

printValueDef :: Purs.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = printPursValName valName <+> equals <+> valDoc
