module LambdaBuffers.Codegen.Plutarch.Print.Derive (hsClassImplPrinters) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Print.InstanceDef qualified as HsInstDef
import LambdaBuffers.Codegen.Haskell.Print.LamVal qualified as HsLamVal
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as H
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as HsSyntax
import LambdaBuffers.Codegen.Haskell.Print.TyDef qualified as HsTyDef
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveFromPlutusDataImplPlutarch, deriveToPlutusDataImplPlutarch)
import LambdaBuffers.Codegen.Plutarch.Print.LamVal (printValueE)
import LambdaBuffers.Codegen.Plutarch.Print.LamVal qualified as PlLamVal
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, defaultLayoutOptions, equals, hardline, layoutPretty, parens, pretty, space, vsep, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Proto.Codegen qualified as P

plutusTypeHsQClassName :: HsSyntax.QClassName
plutusTypeHsQClassName = (H.MkCabalPackageName "plutarch", H.MkModuleName "Plutarch.Internal.PlutusType", H.MkClassName "PlutusType")

hsClassImplPrinters ::
  Map
    H.QClassName
    ( PC.ModuleName ->
      PC.TyDefs ->
      (Doc ann -> Doc ann) ->
      PC.Ty ->
      Either P.InternalError (Doc ann, Set H.QValName)
    )
hsClassImplPrinters =
  Map.fromList
    [
      ( (H.MkCabalPackageName "plutarch", H.MkModuleName "Plutarch.Bool", H.MkClassName "PEq")
      , printDerivePEq
      )
    ,
      ( (H.MkCabalPackageName "plutarch", H.MkModuleName "Plutarch.Builtin", H.MkClassName "PIsData")
      , printDerivePIsData
      )
    ,
      ( (H.MkCabalPackageName "plutarch", H.MkModuleName "Plutarch.TryFrom", H.MkClassName "PTryFrom")
      , printDeriveFromPlutusData
      )
    ,
      ( plutusTypeHsQClassName
      , printDerivePlutusType
      )
    ]

peqMethod :: H.ValueName
peqMethod = H.MkValueName "#=="

-- Plutarch derived classes (Generic, PShow).

-- showClass :: HsSyntax.QClassName
-- showClass = (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Show", HsSyntax.MkClassName "PShow")

-- derivingShowDoc :: Doc ann
-- derivingShowDoc = "deriving anyclass" <+> HsSyntax.printHsQClassName showClass

-- genericClass :: HsSyntax.QClassName
-- genericClass = (HsSyntax.MkCabalPackageName "base", HsSyntax.MkModuleName "GHC.Generics", HsSyntax.MkClassName "Generic")

-- derivingGenericDoc :: Doc ann
-- derivingGenericDoc = "deriving stock" <+> HsSyntax.printHsQClassName genericClass

{- | Deriving PEq.

NOTE(bladyjoker): Doesn't derive the implementation but only uses the underlying PData representation for equality.

```
instance PEq (FooLessTrivial a) where
   (#==) l r = pdata l #== pdata r
```

mkInstanceDoc "\\l r -> (Plutarch.Bool.#==) (Plutarch.Builtin.pdata l) (Plutarch.Builtin.pdata r)"
-}
printDerivePEq :: forall ann. PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set H.QValName)
printDerivePEq _mn _iTyDefs _mkInstanceDoc ty = do
  let implDoc = "\\l r -> (Plutarch.Bool.#==) (Plutarch.Builtin.pdata l) (Plutarch.Builtin.pdata r)" :: Doc ann
      imps =
        Set.fromList
          [ (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Builtin", HsSyntax.MkValueName "pdata")
          , (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Bool", HsSyntax.MkValueName "#==")
          ]
  let instanceDoc = printPEqInstanceDef ty (printValueDef peqMethod implDoc)
  return (instanceDoc, imps)

printPEqInstanceDef :: PC.Ty -> Doc ann -> Doc ann
printPEqInstanceDef ty implDefDoc =
  let headDoc = HsInstDef.printConstraint (H.MkCabalPackageName "plutarch", H.MkModuleName "Plutarch.Bool", H.MkClassName "PEq") ty
      freeVars = HsInstDef.collectTyVars ty
   in case freeVars of
        [] -> "instance" <+> headDoc <+> "where" <> hardline <> space <> space <> implDefDoc
        _ ->
          "instance"
            <+> HsInstDef.printInstanceContext (H.MkCabalPackageName "plutarch", H.MkModuleName "Plutarch.Builtin", H.MkClassName "PIsData") freeVars
            <+> "=>"
            <+> headDoc
            <+> "where" <> hardline <> space <> space <> implDefDoc

{- | Deriving PIsData.

NOTE(bladyjoker): Doesn't derive the implementation but only uses `punsafeCoerce`.

```
instance PIsData (FooLessTrivial a) where
  pdataImpl = punsafeCoerce
  pfromDataImpl = punsafeCoerce
```
-}
printDerivePIsData :: forall ann. PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set H.QValName)
printDerivePIsData _mn _iTyDefs mkInstanceDoc _ty = do
  let imps =
        Set.fromList
          [ (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Unsafe", HsSyntax.MkValueName "punsafeCoerce")
          ]
  let pdataImpl, pfromDataImpl :: Doc ann
      pdataImpl = printValueDef (HsSyntax.MkValueName "pdataImpl") "Plutarch.Unsafe.punsafeCoerce"
      pfromDataImpl = printValueDef (HsSyntax.MkValueName "pfromDataImpl") "Plutarch.Unsafe.punsafeCoerce"
  let instanceDoc = mkInstanceDoc (align $ vsep [pdataImpl, pfromDataImpl])
  return (instanceDoc, imps)

lvPlutusDataBuiltins :: Map LV.ValueName H.QValName
lvPlutusDataBuiltins =
  Map.fromList
    [ ("toPlutusData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "toBuiltinData"))
    , ("fromPlutusData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "fromBuiltinData"))
    , ("casePlutusData", (H.MkCabalPackageName "lbr-plutus", H.MkModuleName "LambdaBuffers.Runtime.Plutus", H.MkValueName "casePlutusData"))
    , ("integerData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Builtins", H.MkValueName "mkI"))
    , ("constrData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Builtins", H.MkValueName "mkConstr"))
    , ("listData", (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx.Builtins", H.MkValueName "mkList"))
    , ("succeedParse", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "Just"))
    , ("failParse", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName "Nothing"))
    , ("bindParse", (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkValueName ">>="))
    ]

lvPlutusDataBuiltinsForPlutusType :: Map LV.ValueName H.QValName
lvPlutusDataBuiltinsForPlutusType =
  Map.fromList
    [ ("toPlutusData", (H.MkCabalPackageName "lbr-plutarch", H.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", H.MkValueName "toPlutusData'"))
    , ("fromPlutusData", (H.MkCabalPackageName "lbr-plutarch", H.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", H.MkValueName "pfromPlutusData"))
    , ("casePlutusData", (H.MkCabalPackageName "lbr-plutarch", H.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", H.MkValueName "pcasePlutusData"))
    , ("integerData", (H.MkCabalPackageName "lbr-plutarch", H.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", H.MkValueName "integerData'"))
    , ("constrData", (H.MkCabalPackageName "lbr-plutarch", H.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", H.MkValueName "constrData'"))
    , ("listData", (H.MkCabalPackageName "lbr-plutarch", H.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", H.MkValueName "listData'"))
    , ("succeedParse", (H.MkCabalPackageName "lbr-plutarch", H.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", H.MkValueName "psucceedParse"))
    , ("failParse", (H.MkCabalPackageName "lbr-plutarch", H.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", H.MkValueName "pfailParse"))
    , ("bindParse", (H.MkCabalPackageName "lbr-plutarch", H.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", H.MkValueName "pbindParse"))
    ]

printDerivePlutusType :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set H.QValName)
printDerivePlutusType mn iTyDefs _mkInstanceDoc ty = do
  toDataE <- deriveToPlutusDataImplPlutarch mn iTyDefs ty
  fromDataE <- deriveFromPlutusDataImplPlutarch mn iTyDefs ty
  let additionalImps =
        Set.fromList
          [ (HsSyntax.MkCabalPackageName "plutarch", HsSyntax.MkModuleName "Plutarch.Prelude", HsSyntax.MkValueName "#")
          ]
  (pconImplDoc, imps) <- LV.runPrint lvPlutusDataBuiltinsForPlutusType (HsLamVal.printValueE toDataE)
  (pmatchImplDoc, imps') <- LV.runPrint lvPlutusDataBuiltinsForPlutusType (PlLamVal.printValueE fromDataE)
  let instanceDoc =
        printPlutusTypeInstanceDef
          ty
          ( align $
              vsep
                [ printValueDef (H.MkValueName "pcon'") pconImplDoc
                , "pmatch' pd f =" <+> parens "Plutarch.Prelude.#" <+> parens (dirtyHack pmatchImplDoc) <+> "pd"
                ]
          )
  return (instanceDoc, imps' <> imps <> additionalImps)
  where
    docToText :: Doc ann -> Text
    docToText = renderStrict . layoutPretty defaultLayoutOptions

    -- TODO(bladyjoker): THe `fromData` implementation is trying to construct a term, which for Plutarch means `pcon`. However, this is 'pmatch' implementation which is NOT really exactly 'fromData', and has a different type signature for which we do this. I'm sorry.
    dirtyHack :: Doc ann -> Doc ann
    dirtyHack = pretty . Text.replace "Plutarch.Prelude.pcon " "f " . docToText

printPlutusTypeInstanceDef :: PC.Ty -> Doc ann -> Doc ann
printPlutusTypeInstanceDef ty implDefDoc =
  let headDoc = HsInstDef.printConstraint plutusTypeHsQClassName ty
      freeVars = HsInstDef.collectTyVars ty
      pinnerDefDoc = "type PInner" <+> HsTyDef.printTyInner ty <+> "=" <+> "Plutarch.Builtin.PData"
   in case freeVars of
        [] ->
          "instance"
            <+> headDoc
            <+> "where"
              <> hardline
              <> space
              <> space
              <> pinnerDefDoc
              <> hardline
              <> space
              <> space
              <> implDefDoc
        _ ->
          "instance"
            <+> HsInstDef.printInstanceContext (H.MkCabalPackageName "plutarch", H.MkModuleName "Plutarch.Builtin", H.MkClassName "PIsData") freeVars
            <+> "=>"
            <+> headDoc
            <+> "where"
              <> hardline
              <> space
              <> space
              <> pinnerDefDoc
              <> hardline
              <> space
              <> space
              <> implDefDoc

printValueDef :: H.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = H.printHsValName valName <+> equals <+> valDoc

fromPlutusDataClassMethodName :: H.ValueName
fromPlutusDataClassMethodName = H.MkValueName "fromBuiltinData"

builtinDataToDataRef :: H.QValName
builtinDataToDataRef = (H.MkCabalPackageName "plutus-tx", H.MkModuleName "PlutusTx", H.MkValueName "builtinDataToData")

printDeriveFromPlutusData :: PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> Either P.InternalError (Doc ann, Set H.QValName)
printDeriveFromPlutusData mn iTyDefs mkInstanceDoc ty = do
  valE <- deriveFromPlutusDataImplPlutarch mn iTyDefs ty
  (implDoc, imps) <- LV.runPrint lvPlutusDataBuiltins (printValueE valE)
  let instanceDoc = mkInstanceDoc (printValueDef fromPlutusDataClassMethodName implDoc)
  return
    ( instanceDoc
    , Set.singleton builtinDataToDataRef <> imps
    )
