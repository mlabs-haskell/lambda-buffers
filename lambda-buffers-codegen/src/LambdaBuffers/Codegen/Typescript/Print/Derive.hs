module LambdaBuffers.Codegen.Typescript.Print.Derive (tsEqClass, printDeriveEq, printDeriveToPlutusData, printDeriveFromPlutusData, printDeriveJson) where

import Control.Lens (view)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Codegen.LamVal.Eq (deriveEqImpl, deriveNeqImpl)
import LambdaBuffers.Codegen.LamVal.Json (deriveFromJsonImpl, deriveToJsonImpl)
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveFromPlutusDataImpl, deriveToPlutusDataImpl)
import LambdaBuffers.Codegen.Typescript.Print.InstanceDef (ExportInstanceDecl (ExportConstInstanceDecl, ExportFunctionInstanceDecl), printInstanceDict)
import LambdaBuffers.Codegen.Typescript.Print.LamVal (Builtin (Builtin, OverloadedBuiltin), printValueE, qualifiedValName)
import LambdaBuffers.Codegen.Typescript.Print.Names (printTsValName)
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (
  Doc,
  align,
  colon,
  comma,
  -- line, pretty,

  defaultLayoutOptions,
  equals,
  indent,
  layoutPretty,
  lbrace,
  rbrace,
  vsep,
  (<+>),
 )
import Proto.Codegen qualified as P

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

lvEqBuiltins :: LV.PrintRead Builtin
lvEqBuiltins = LV.MkPrintRead $ \(tys, refName) ->
  case (refName, tys) of
    ("eq", [ty]) -> do
      ty' <- lamTy2PCTy ty
      return $
        OverloadedBuiltin
          ( Ts.normalValName
              "prelude"
              "Prelude"
              -- "eqDict"
              (PrettyPrinter.Text.renderStrict (layoutPretty defaultLayoutOptions (printInstanceDict tsEqClass ty')))
          )
          0 -- index in the list of substitutions for the type we're overloading on
          ".eq"
    ("true", _) -> Just $ Builtin $ Ts.primValName "true"
    ("false", _) -> Just $ Builtin $ Ts.primValName "false"
    ("and", _) -> Just $ Builtin $ Ts.normalValName "prelude" "Prelude" "and"
    ("not", _) -> Just $ Builtin $ Ts.primValName "!"
    _ -> Nothing

eqClassMethodName :: Ts.ValueName
eqClassMethodName = Ts.MkValueName "eq"

neqClassMethodName :: Ts.ValueName
neqClassMethodName = Ts.MkValueName "neq"

tsEqClass :: Ts.QClassName
tsEqClass = (Ts.MkPackageName "prelude", Ts.MkModuleName "Prelude", Ts.MkClassName "Eq")

printDeriveEq :: PC.ModuleName -> PC.TyDefs -> ExportInstanceDecl (Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Ts.QValName)
printDeriveEq mn iTyDefs exportInstanceDeclDoc ty = do
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
      instanceDoc =
        case exportInstanceDeclDoc of
          ExportConstInstanceDecl doc ->
            doc
              <+> equals
              <+> vsep
                [ lbrace
                , indent 2 eqValueDefDoc
                , rbrace
                ]
          ExportFunctionInstanceDecl doc ->
            doc
              <> vsep
                [ lbrace
                , indent 2 $
                    vsep
                      [ "return" <+> lbrace
                      , indent 2 eqValueDefDoc
                      , rbrace
                      ]
                , rbrace
                ]

  return (instanceDoc, Set.map (view qualifiedValName) $ eqImports <> neqImports)

lvPlutusDataBuiltins :: LV.PrintRead Builtin
lvPlutusDataBuiltins = LV.MkPrintRead $ \(_ty, refName) ->
  Map.lookup refName $
    Map.fromList
      [ ("toPlutusData", Builtin $ Ts.normalValName "cardano-transaction-lib" "Ctl.Internal.ToData" "toData")
      , ("fromPlutusData", Builtin $ Ts.normalValName "cardano-transaction-lib" "Ctl.Internal.FromData" "fromData")
      , ("casePlutusData", Builtin $ Ts.normalValName "lbr-plutus" "LambdaBuffers.Runtime.Plutus" "casePlutusData")
      , ("integerData", Builtin $ Ts.normalValName "cardano-transaction-lib" "Ctl.Internal.Types.PlutusData" "Integer")
      , ("constrData", Builtin $ Ts.normalValName "lbr-plutus" "LambdaBuffers.Runtime.Plutus" "pdConstr")
      , ("listData", Builtin $ Ts.normalValName "cardano-transaction-lib" "Ctl.Internal.Types.PlutusData" "List")
      , ("succeedParse", Builtin $ Ts.normalValName "maybe" "Data.Maybe" "Just")
      , ("failParse", Builtin $ Ts.normalValName "maybe" "Data.Maybe" "Nothing")
      , ("bindParse", Builtin $ Ts.normalValName "prelude" "Prelude" ">>=")
      ]

toPlutusDataClassMethodName :: Ts.ValueName
toPlutusDataClassMethodName = Ts.MkValueName "toData"

fromPlutusDataClassMethodName :: Ts.ValueName
fromPlutusDataClassMethodName = Ts.MkValueName "fromData"

printDeriveToPlutusData :: PC.ModuleName -> PC.TyDefs -> ExportInstanceDecl (Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Ts.QValName)
printDeriveToPlutusData mn iTyDefs exportInstanceDeclDoc ty = do
  valE <- deriveToPlutusDataImpl mn iTyDefs ty
  (implDoc, imports) <- LV.runPrint lvPlutusDataBuiltins (printValueE valE)
  let valueDefDoc = printValueDef toPlutusDataClassMethodName implDoc
      instanceDoc = case exportInstanceDeclDoc of
        ExportConstInstanceDecl doc -> doc <+> valueDefDoc
        ExportFunctionInstanceDecl doc -> doc <+> lbrace <+> valueDefDoc <+> rbrace
  return
    ( instanceDoc
    , Set.map (view qualifiedValName) imports
    )

printDeriveFromPlutusData :: PC.ModuleName -> PC.TyDefs -> ExportInstanceDecl (Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Ts.QValName)
printDeriveFromPlutusData mn iTyDefs exportInstanceDeclDoc ty = do
  valE <- deriveFromPlutusDataImpl mn iTyDefs ty
  (implDoc, imports) <- LV.runPrint lvPlutusDataBuiltins (printValueE valE)
  let valueDefDoc = printValueDef fromPlutusDataClassMethodName implDoc
      instanceDoc = case exportInstanceDeclDoc of
        ExportConstInstanceDecl doc -> doc <+> valueDefDoc
        ExportFunctionInstanceDecl doc -> doc <+> lbrace <+> valueDefDoc <+> rbrace

  return
    ( instanceDoc
    , Set.map (view qualifiedValName) imports
    )

printValueDef :: Ts.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = printTsValName valName <+> equals <+> valDoc

-- | LambdaBuffers.Codegen.LamVal.Json specification printing
lvJsonBuiltins :: LV.PrintRead Builtin
lvJsonBuiltins = LV.MkPrintRead $ \(_ty, refName) ->
  Map.lookup refName $
    Map.fromList
      [ ("toJson", Builtin $ Ts.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "toJson")
      , ("fromJson", Builtin $ Ts.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "fromJson")
      , ("jsonObject", Builtin $ Ts.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "jsonObject")
      , ("jsonConstructor", Builtin $ Ts.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "jsonConstructor")
      , ("jsonArray", Builtin $ Ts.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "jsonArray")
      , ("caseJsonConstructor", Builtin $ Ts.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "caseJsonConstructor")
      , ("caseJsonArray", Builtin $ Ts.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "caseJsonArray")
      , ("caseJsonObject", Builtin $ Ts.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "caseJsonObject")
      , ("jsonField", Builtin $ Ts.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "jsonField")
      , ("succeedParse", Builtin $ Ts.normalValName "either" "Data.Either" "Right")
      , ("failParse", Builtin $ Ts.normalValName "lbr-prelude" "LambdaBuffers.Runtime.Prelude" "fail")
      , ("bindParse", Builtin $ Ts.normalValName "prelude" "Prelude" ">>=")
      ]

toJsonClassMethodName :: Ts.ValueName
toJsonClassMethodName = Ts.MkValueName "toJson"

fromJsonClassMethodName :: Ts.ValueName
fromJsonClassMethodName = Ts.MkValueName "fromJson"

printDeriveJson :: PC.ModuleName -> PC.TyDefs -> ExportInstanceDecl (Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Ts.QValName)
printDeriveJson mn iTyDefs exportInstanceDeclDoc ty = do
  toJsonValE <- deriveToJsonImpl mn iTyDefs ty
  (toJsonImplDoc, impsA) <- LV.runPrint lvJsonBuiltins (printValueE toJsonValE)
  fromJsonValE <- deriveFromJsonImpl mn iTyDefs ty
  (fromJsonImplDoc, impsB) <- LV.runPrint lvJsonBuiltins (printValueE fromJsonValE)

  let valueDefDoc =
        align $
          vsep
            [ printValueDef toJsonClassMethodName toJsonImplDoc
            , printValueDef fromJsonClassMethodName fromJsonImplDoc
            ]
      instanceDoc = case exportInstanceDeclDoc of
        ExportConstInstanceDecl doc -> doc <+> valueDefDoc
        ExportFunctionInstanceDecl doc -> doc <+> lbrace <+> valueDefDoc <+> rbrace

  return
    ( instanceDoc
    , Set.map (view qualifiedValName) $ impsA <> impsB
    )
