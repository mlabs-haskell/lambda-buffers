module LambdaBuffers.Codegen.Haskell.Print.Derive (printDeriveEq, printDeriveToPlutusData) where

import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Print.LamVal (printImplementation)
import LambdaBuffers.Codegen.Haskell.Print.Names (printHsQValName, printHsValName)
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl)
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveToPlutusDataImpl)
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, align, equals, group, sep, (<+>))

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
    [ ("toPlutusData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "toData"))
    , ("integerData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "I"))
    , ("constrData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "Constr"))
    , ("listData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "List"))
    ]

dataToBuiltinDataRef :: H.QValName
dataToBuiltinDataRef = (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "dataToBuiltinData")

dotOp :: H.QValName
dotOp = (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName ".")

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
          let dataToBuiltinDataDoc = printValueApp dotOp [printHsQValName dataToBuiltinDataRef, implDoc]
              instanceDoc = mkInstanceDoc (printValueDef toPlutusDataClassMethodName dataToBuiltinDataDoc)
           in Right
                ( instanceDoc
                , Set.insert dataToBuiltinDataRef $ Set.fromList . toList $ lvPlutusDataBuiltins
                )

printValueDef :: H.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = printHsValName valName <+> equals <+> valDoc

printValueApp :: H.QValName -> [Doc ann] -> Doc ann
printValueApp fVal aDocs = align . group $ (printHsQValName fVal <+> (align . group . sep $ aDocs))
