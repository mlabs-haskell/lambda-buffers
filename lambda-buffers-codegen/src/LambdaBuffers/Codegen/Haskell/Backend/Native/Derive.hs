module LambdaBuffers.Codegen.Haskell.Backend.Native.Derive (printValue, printDeriveEqBase, printDeriveToPlutusData, printDeriveFromPlutusData, printDeriveJson, hsClassImplPrinters) where

import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import LambdaBuffers.Codegen.Haskell.Backend (MonadHaskellBackend)
import LambdaBuffers.Codegen.Haskell.Backend.Native.LamVal qualified as Native
import LambdaBuffers.Codegen.Haskell.Print.LamVal qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as H
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as Haskell
import LambdaBuffers.Codegen.LamVal qualified as Lv
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl)
import LambdaBuffers.Codegen.LamVal.Json (deriveFromJsonImpl, deriveToJsonImpl)
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveFromPlutusDataImpl, deriveToPlutusDataImpl)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, equals, vsep, (<+>))
import Proto.Codegen_Fields qualified as P

hsClassImplPrinters ::
  MonadHaskellBackend t m =>
  Map
    H.QClassName
    ( PC.ModuleName ->
      PC.TyDefs ->
      (Doc ann -> m (Doc ann)) ->
      PC.Ty ->
      m (Doc ann)
    )
hsClassImplPrinters =
  Map.fromList
    [
      ( (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkClassName "Eq")
      , printDeriveEqBase
      )
    ,
      ( (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkClassName "ToData")
      , printDeriveToPlutusData
      )
    ,
      ( (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkClassName "FromData")
      , printDeriveFromPlutusData
      )
    ,
      ( (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkClassName "Json")
      , printDeriveJson
      )
    ]

printValue :: (Lv.Ref -> Maybe Haskell.QValName) -> Lv.ValueE -> Either LV.PrintError (Doc ann, Set Haskell.QValName)
printValue builtins valE = LV.runPrint (LV.Context builtins Native.lamValContext) (Haskell.printValueE valE)

eqClassMethodName :: H.ValueName
eqClassMethodName = H.MkValueName "=="

lvEqBuiltinsBase :: Lv.Ref -> Maybe Haskell.QValName
lvEqBuiltinsBase (_ty, refName) =
  Map.lookup refName $
    Map.fromList
      [ ("eq", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "=="))
      , ("and", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "&&"))
      , ("true", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "True"))
      , ("false", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "False"))
      ]

printDeriveEqBase :: MonadHaskellBackend t m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> m (Doc ann)) -> PC.Ty -> m (Doc ann)
printDeriveEqBase mn iTyDefs mkInstanceDoc ty = do
  case deriveEqImpl mn iTyDefs ty of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Deriving Prelude.Eq LamVal implementation from a type failed with: " <> err ^. P.msg)
    Right valE -> do
      case printValue lvEqBuiltinsBase valE of
        Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Interpreting LamVal into Haskell failed with: " <> err ^. P.msg)
        Right (implDoc, imps) -> do
          instanceDoc <- mkInstanceDoc (printValueDef eqClassMethodName implDoc)
          for_ imps Print.importValue
          return instanceDoc

lvPlutusDataBuiltins :: Lv.Ref -> Maybe Haskell.QValName
lvPlutusDataBuiltins (_ty, refName) =
  Map.lookup refName $
    Map.fromList
      [ ("toPlutusData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "toBuiltinData"))
      , ("fromPlutusData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "fromBuiltinData"))
      , ("casePlutusData", (H.MkCabalPackageName "lbr-plutus", H.MkModuleName "LambdaBuffers.Runtime.Plutus", H.MkValueName "casePlutusData"))
      , ("integerData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Builtins", H.MkValueName "mkI"))
      , ("constrData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Builtins", H.MkValueName "mkConstr"))
      , ("listData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Builtins", H.MkValueName "mkList"))
      , ("succeedParse", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Maybe", H.MkValueName "Just"))
      , ("failParse", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Maybe", H.MkValueName "Nothing"))
      , ("bindParse", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Prelude", H.MkValueName ">>="))
      ]

toPlutusDataClassMethodName :: H.ValueName
toPlutusDataClassMethodName = H.MkValueName "toBuiltinData"

printDeriveToPlutusData :: MonadHaskellBackend t m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> m (Doc ann)) -> PC.Ty -> m (Doc ann)
printDeriveToPlutusData mn iTyDefs mkInstanceDoc ty = do
  case deriveToPlutusDataImpl mn iTyDefs ty of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Deriving Plutus.V1.PlutusData LamVal implementation from a type failed with: " <> err ^. P.msg)
    Right valE -> do
      case printValue lvPlutusDataBuiltins valE of
        Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Interpreting LamVal into Haskell failed with: " <> err ^. P.msg)
        Right (implDoc, imps) -> do
          instanceDoc <- mkInstanceDoc (printValueDef toPlutusDataClassMethodName implDoc)
          for_ imps Print.importValue
          return instanceDoc

printValueDef :: H.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = H.printHsValName valName <+> equals <+> valDoc

fromPlutusDataClassMethodName :: H.ValueName
fromPlutusDataClassMethodName = H.MkValueName "fromBuiltinData"

builtinDataToDataRef :: H.QValName
builtinDataToDataRef = (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "builtinDataToData")

printDeriveFromPlutusData :: MonadHaskellBackend t m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> m (Doc ann)) -> PC.Ty -> m (Doc ann)
printDeriveFromPlutusData mn iTyDefs mkInstanceDoc ty = do
  case deriveFromPlutusDataImpl mn iTyDefs ty of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Deriving Plutus.V1.PlutusData LamVal implementation from a type failed with: " <> err ^. P.msg)
    Right valE -> do
      case printValue lvPlutusDataBuiltins valE of
        Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Interpreting LamVal into Haskell failed with: " <> err ^. P.msg)
        Right (implDoc, imps) -> do
          instanceDoc <- mkInstanceDoc (printValueDef fromPlutusDataClassMethodName implDoc)
          Print.importValue builtinDataToDataRef
          for_ imps Print.importValue
          return instanceDoc

-- | LambdaBuffers.Codegen.LamVal.Json specification printing
lvJsonBuiltins :: Lv.Ref -> Maybe Haskell.QValName
lvJsonBuiltins (_ty, refName) =
  Map.lookup refName $
    Map.fromList
      [ ("toJson", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "toJson"))
      , ("fromJson", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "fromJson"))
      , ("jsonObject", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "jsonObject"))
      , ("jsonConstructor", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "jsonConstructor"))
      , ("jsonArray", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "jsonArray"))
      , ("caseJsonConstructor", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "caseJsonConstructor"))
      , ("caseJsonArray", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "caseJsonArray"))
      , ("caseJsonObject", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "caseJsonObject"))
      , ("jsonField", (H.MkCabalPackageName "lbr-prelude", H.MkModuleName "LambdaBuffers.Runtime.Prelude", H.MkValueName "jsonField"))
      , ("succeedParse", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "return"))
      , ("failParse", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "fail"))
      , ("bindParse", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName ">>="))
      ]

toJsonClassMethodName :: H.ValueName
toJsonClassMethodName = H.MkValueName "toJson"

fromJsonClassMethodName :: H.ValueName
fromJsonClassMethodName = H.MkValueName "fromJson"

printDeriveJson :: MonadHaskellBackend t m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> m (Doc ann)) -> PC.Ty -> m (Doc ann)
printDeriveJson mn iTyDefs mkInstanceDoc ty = do
  let resOrErr = do
        toJsonValE <- deriveToJsonImpl mn iTyDefs ty
        (toJsonImplDoc, impsA) <- printValue lvJsonBuiltins toJsonValE
        fromJsonValE <- deriveFromJsonImpl mn iTyDefs ty
        (fromJsonImplDoc, impsB) <- printValue lvJsonBuiltins fromJsonValE
        return (toJsonImplDoc, fromJsonImplDoc, impsA <> impsB)

  (toJsonImplDoc, fromJsonImplDoc, imps) <- case resOrErr of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Deriving Prelude.Json LamVal implementation from a type failed with: " <> err ^. P.msg)
    Right res -> return res

  instanceDoc <-
    mkInstanceDoc
      ( align $
          vsep
            [ printValueDef toJsonClassMethodName toJsonImplDoc
            , printValueDef fromJsonClassMethodName fromJsonImplDoc
            ]
      )

  for_ imps Print.importValue

  return instanceDoc
