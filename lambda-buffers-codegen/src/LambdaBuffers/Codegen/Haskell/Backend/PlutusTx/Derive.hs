module LambdaBuffers.Codegen.Haskell.Backend.PlutusTx.Derive (printDeriveEqPlutusTx, printDeriveToPlutusData, printDeriveFromPlutusData, instancePrinters) where

import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import LambdaBuffers.Codegen.Haskell.Backend (MonadHaskellBackend)
import LambdaBuffers.Codegen.Haskell.Backend.PlutusTx.LamVal qualified as PlutusTx
import LambdaBuffers.Codegen.Haskell.Print.LamVal qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as H
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as Haskell
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl)
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveFromPlutusDataImpl, deriveToPlutusDataImpl)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, equals, hardline, (<+>))
import Proto.Codegen_Fields qualified as P

instancePrinters ::
  MonadHaskellBackend t m =>
  Map
    H.QClassName
    ( PC.ModuleName ->
      PC.TyDefs ->
      (Doc ann -> m (Doc ann)) ->
      PC.Ty ->
      m (Doc ann)
    )
instancePrinters =
  Map.fromList
    [
      ( (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Eq", H.MkClassName "Eq")
      , printDeriveEqPlutusTx
      )
    ,
      ( (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkClassName "ToData")
      , printDeriveToPlutusData
      )
    ,
      ( (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkClassName "FromData")
      , printDeriveFromPlutusData
      )
    ]

printValue :: (LV.Ref -> Maybe Haskell.QValName) -> LV.ValueE -> Either LV.PrintError (Doc ann, Set Haskell.QValName)
printValue builtins valE = LV.runPrint (LV.Context builtins PlutusTx.lamValContext) (Haskell.printValueE valE)

lvEqBuiltinsPlutusTx :: LV.Ref -> Maybe Haskell.QValName
lvEqBuiltinsPlutusTx (_ty, refName) =
  Map.lookup refName $
    Map.fromList
      [ ("eq", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Eq", H.MkValueName "=="))
      , ("and", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Bool", H.MkValueName "&&"))
      , ("true", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Bool", H.MkValueName "True"))
      , ("false", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Bool", H.MkValueName "False"))
      ]

eqClassMethodName :: H.ValueName
eqClassMethodName = H.MkValueName "=="

printDeriveEqPlutusTx :: MonadHaskellBackend t m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> m (Doc ann)) -> PC.Ty -> m (Doc ann)
printDeriveEqPlutusTx mn iTyDefs mkInstanceDoc ty = do
  case deriveEqImpl mn iTyDefs ty of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Deriving Prelude.Eq LamVal implementation from a type failed with: " <> err ^. P.msg)
    Right valE -> do
      case printValue lvEqBuiltinsPlutusTx valE of
        Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Interpreting LamVal into Haskell failed with: " <> err ^. P.msg)
        Right (implDoc, imps) -> do
          instanceDoc <- mkInstanceDoc (align $ printInlineable eqClassMethodName <> hardline <> printValueDef eqClassMethodName implDoc)
          for_ imps Print.importValue
          return instanceDoc

printInlineable :: H.ValueName -> Doc ann
printInlineable valName = "{-# INLINABLE" <+> H.printHsValName valName <+> "#-}"

lvPlutusDataBuiltins :: LV.Ref -> Maybe Haskell.QValName
lvPlutusDataBuiltins (_ty, refName) =
  Map.lookup refName $
    Map.fromList
      [ ("toPlutusData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "toBuiltinData"))
      , ("fromPlutusData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "fromBuiltinData"))
      , ("casePlutusData", (H.MkCabalPackageName "lbr-plutustx", H.MkModuleName "LambdaBuffers.Runtime.PlutusTx.LamVal", H.MkValueName "casePlutusData"))
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
          instanceDoc <- mkInstanceDoc (align $ printInlineable toPlutusDataClassMethodName <> hardline <> printValueDef toPlutusDataClassMethodName implDoc)
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
          instanceDoc <- mkInstanceDoc (align $ printInlineable fromPlutusDataClassMethodName <> hardline <> printValueDef fromPlutusDataClassMethodName implDoc)
          Print.importValue builtinDataToDataRef
          for_ imps Print.importValue
          return instanceDoc
