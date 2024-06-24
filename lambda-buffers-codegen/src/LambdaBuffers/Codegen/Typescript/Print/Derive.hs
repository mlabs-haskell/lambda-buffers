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
  _other ->
    -- NOTE(jaredponn): hopefully this never happens...
    Nothing

instanceDictIdent :: Ts.PkgMap -> Ts.QClassName -> PC.Ty -> Text
instanceDictIdent pkgMap className ty =
  Lens.view (dict . Lens.to (PrettyPrinter.Text.renderStrict . layoutPretty defaultLayoutOptions)) $
    printInstanceDict pkgMap className ty

lvEqBuiltins :: Ts.PkgMap -> LV.Context Builtin ()
lvEqBuiltins pkgMap =
  LV.Context
    ( \(tys, refName) ->
        case (refName, tys) of
          ("eq", [ty]) -> do
            ty' <- lamTy2PCTy ty
            return $
              OverloadedBuiltin
                (Ts.primValName $ instanceDictIdent pkgMap tsEqClass ty')
                0 -- index in the list of substitutions for the type we're overloading on
                ".eq"
          ("true", _) -> Just $ Builtin $ Ts.primValName "true"
          ("false", _) -> Just $ Builtin $ Ts.primValName "false"
          ("and", _) -> Just $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "and"
          ("not", _) -> Just $ Builtin $ Ts.primValName "!"
          _other -> Nothing
    )
    ()

eqClassMethodName :: Ts.ValueName
eqClassMethodName = Ts.MkValueName "eq"

neqClassMethodName :: Ts.ValueName
neqClassMethodName = Ts.MkValueName "neq"

tsEqClass :: Ts.QClassName
tsEqClass = (Ts.MkPackageName "lbr-prelude", Ts.MkModuleName "LbrPrelude", Ts.MkClassName "Eq")

printDeriveEq :: Ts.PkgMap -> PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Ts.QValName)
printDeriveEq pkgMap mn iTyDefs mkExportInstanceDeclDoc ty = do
  eqValE <- deriveEqImpl mn iTyDefs ty
  neqValE <- deriveNeqImpl mn iTyDefs ty
  (eqImplDoc, eqImports) <- LV.runPrint (lvEqBuiltins pkgMap) (printValueE eqValE)
  (neqImplDoc, neqImports) <- LV.runPrint (lvEqBuiltins pkgMap) (printValueE neqValE)
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

lvPlutusDataBuiltins :: Ts.PkgMap -> LV.Context Builtin ()
lvPlutusDataBuiltins pkgMap =
  LV.Context
    ( \(tys, refName) ->
        case (refName, tys) of
          ("toPlutusData", [ty]) -> do
            ty' <- lamTy2PCTy ty
            return $
              OverloadedBuiltin
                (Ts.primValName $ instanceDictIdent pkgMap tsIsPlutusDataClass ty')
                0 -- index in the list of substitutions for the type we're overloading on
                ".toData"
          ("fromPlutusData", [ty]) -> do
            ty' <- lamTy2PCTy ty
            return $
              OverloadedBuiltin
                (Ts.primValName $ instanceDictIdent pkgMap tsIsPlutusDataClass ty')
                0 -- index in the list of substitutions for the type we're overloading on
                ".fromData"
          ("casePlutusData", _) -> Just $ Builtin $ Ts.normalValName "lbr-plutus/Runtime.js" "LbrPlutusRuntime" "casePlutusData"
          ("integerData", _) -> Just $ Builtin $ Ts.normalValName "lbr-plutus/Runtime.js" "LbrPlutusRuntime" "integerData"
          ("constrData", _) -> Just $ Builtin $ Ts.normalValName "lbr-plutus/Runtime.js" "LbrPlutusRuntime" "constrData"
          ("listData", _) -> Just $ Builtin $ Ts.normalValName "lbr-plutus/Runtime.js" "LbrPlutusRuntime" "listData"
          ("succeedParse", _) -> Just $ Builtin $ Ts.normalValName "lbr-plutus/Runtime.js" "LbrPlutusRuntime" "succeedParse"
          ("failParse", _) -> Just $ Builtin $ Ts.normalValName "lbr-plutus/Runtime.js" "LbrPlutusRuntime" "failParse('PlutusData parse failed')" -- TODO(jaredponn): bit of a hack to call the function @failParse@
          ("bindParse", _) -> Just $ Builtin $ Ts.normalValName "lbr-plutus/Runtime.js" "LbrPlutusRuntime" "bindParse"
          _other -> Nothing
    )
    ()

toPlutusDataClassMethodName :: Ts.ValueName
toPlutusDataClassMethodName = Ts.MkValueName "toData"

fromPlutusDataClassMethodName :: Ts.ValueName
fromPlutusDataClassMethodName = Ts.MkValueName "fromData"

tsIsPlutusDataClass :: Ts.QClassName
tsIsPlutusDataClass = (Ts.MkPackageName "lbr-plutus/PlutusData.js", Ts.MkModuleName "LbrPlutusPD", Ts.MkClassName "IsPlutusData")

printDeriveIsPlutusData :: Ts.PkgMap -> PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Ts.QValName)
printDeriveIsPlutusData pkgMap mn iTyDefs mkExportInstanceDeclDoc ty = do
  toPlutusDataValE <- deriveToPlutusDataImpl mn iTyDefs ty
  fromPlutusDataValE <- deriveFromPlutusDataImpl mn iTyDefs ty

  (toDataImplDoc, toDataImports) <- LV.runPrint (lvPlutusDataBuiltins pkgMap) (printValueE toPlutusDataValE)
  (fromDataImplDoc, fromDataImports) <- LV.runPrint (lvPlutusDataBuiltins pkgMap) (printValueE fromPlutusDataValE)

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
lvJsonBuiltins :: Ts.PkgMap -> LV.Context Builtin ()
lvJsonBuiltins pkgMap =
  LV.Context
    ( \(tys, refName) ->
        case (refName, tys) of
          ("toJson", [ty]) -> do
            ty' <- lamTy2PCTy ty
            return $
              OverloadedBuiltin
                (Ts.primValName $ instanceDictIdent pkgMap tsJsonClass ty')
                0 -- index in the list of substitutions for the type we're overloading on
                ".toJson"
          ("fromJson", [ty]) -> do
            ty' <- lamTy2PCTy ty
            return $
              OverloadedBuiltin
                (Ts.primValName (instanceDictIdent pkgMap tsJsonClass ty'))
                0 -- index in the list of substitutions for the type we're overloading on
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
                      "caseJsonConstructor" <> surround (Typescript.Print.Ty.printTyInner pkgMap ty') langle rangle
                  )
          ("caseJsonArray", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "caseJsonArray"
          ("caseJsonObject", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "caseJsonObject"
          ("jsonField", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "jsonField"
          ("succeedParse", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "succeedParse"
          ("failParse", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "failParse"
          ("bindParse", _) -> return $ Builtin $ Ts.normalValName "lbr-prelude" "LbrPrelude" "bindParse"
          _other -> Nothing
    )
    ()

toJsonClassMethodName :: Ts.ValueName
toJsonClassMethodName = Ts.MkValueName "toJson"

fromJsonClassMethodName :: Ts.ValueName
fromJsonClassMethodName = Ts.MkValueName "fromJson"

printDeriveJson :: Ts.PkgMap -> PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set Ts.QValName)
printDeriveJson pkgMap mn iTyDefs mkExportInstanceDeclDoc ty = do
  toJsonValE <- deriveToJsonImpl mn iTyDefs ty
  (toJsonImplDoc, impsA) <- LV.runPrint (lvJsonBuiltins pkgMap) (printValueE toJsonValE)
  fromJsonValE <- deriveFromJsonImpl mn iTyDefs ty
  (fromJsonImplDoc, impsB) <- LV.runPrint (lvJsonBuiltins pkgMap) (printValueE fromJsonValE)

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
