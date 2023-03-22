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
import LambdaBuffers.Codegen.Purescript.Print.Names (printPursQValName, printPursValName)
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purs
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, align, equals, group, sep, (<+>))

lvEqBuiltins :: Map LV.ValueName (Purs.PackageName, Purs.ModuleName, Purs.ValueName)
lvEqBuiltins =
  Map.fromList
    [ ("eq", (Purs.MkPackageName "prelude", Purs.MkModuleName "Prelude", Purs.MkValueName "=="))
    , ("and", (Purs.MkPackageName "prelude", Purs.MkModuleName "Prelude", Purs.MkValueName "&&"))
    , ("true", (Purs.MkPackageName "prelude", Purs.MkModuleName "Prelude", Purs.MkValueName "True"))
    , ("false", (Purs.MkPackageName "base", Purs.MkModuleName "Prelude", Purs.MkValueName "False"))
    ]

eqClassMethodName :: Purs.ValueName
eqClassMethodName = Purs.MkValueName "=="

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
    [ ("toPlutusData", (Purs.MkPackageName "cardano-transaction-lib", Purs.MkModuleName "Ctl.Internal.ToData", Purs.MkValueName "toData"))
    , ("integerData", (Purs.MkPackageName "cardano-transaction-lib", Purs.MkModuleName "Ctl.Internal.ToData", Purs.MkValueName "I"))
    , ("constrData", (Purs.MkPackageName "cardano-transaction-lib", Purs.MkModuleName "Ctl.Internal.ToData", Purs.MkValueName "Constr"))
    , ("listData", (Purs.MkPackageName "cardano-transaction-lib", Purs.MkModuleName "Ctl.Internal.ToData", Purs.MkValueName "List"))
    ]

dataToBuiltinDataRef :: Purs.QValName
dataToBuiltinDataRef = (Purs.MkPackageName "plutus-tx", Purs.MkModuleName "PlutusTx", Purs.MkValueName "dataToBuiltinData")

dotOp :: Purs.QValName
dotOp = (Purs.MkPackageName "prelude", Purs.MkModuleName "Prelude", Purs.MkValueName ".")

toPlutusDataClassMethodName :: Purs.ValueName
toPlutusDataClassMethodName = Purs.MkValueName "toBuiltinData"

-- TODO: Handle errors properly.
printDeriveToPlutusData :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either Text (Doc ann, Set Purs.QValName)
printDeriveToPlutusData mn iTyDefs mkInstanceDoc ty =
  case deriveToPlutusDataImpl mn iTyDefs ty of
    Left err -> Left $ Text.pack err
    Right valE ->
      case printImplementation lvPlutusDataBuiltins valE of
        Left err -> Left $ Text.pack $ show err
        Right implDoc ->
          let dataToBuiltinDataDoc = printValueApp dotOp [printPursQValName dataToBuiltinDataRef, implDoc]
              instanceDoc = mkInstanceDoc (printValueDef toPlutusDataClassMethodName dataToBuiltinDataDoc)
           in Right
                ( instanceDoc
                , Set.insert dataToBuiltinDataRef $ Set.fromList . toList $ lvPlutusDataBuiltins
                )

printValueDef :: Purs.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = printPursValName valName <+> equals <+> valDoc

printValueApp :: Purs.QValName -> [Doc ann] -> Doc ann
printValueApp fVal aDocs = align . group $ (printPursQValName fVal <+> (align . group . sep $ aDocs))
