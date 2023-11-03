module LambdaBuffers.Codegen.Plutarch.Print.Derive (hsClassImplPrinters) where

import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Print (MonadPrint)
import LambdaBuffers.Codegen.Haskell.Print.InstanceDef qualified as HsInstDef
import LambdaBuffers.Codegen.Haskell.Print.InstanceDef qualified as HsSyntax
import LambdaBuffers.Codegen.Haskell.Print.LamVal qualified as HsLamVal
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as HsSyntax
import LambdaBuffers.Codegen.Haskell.Print.TyDef qualified as HsTyDef
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveFromPlutusDataImplPlutarch, deriveToPlutusDataImplPlutarch)
import LambdaBuffers.Codegen.Plutarch.Print.LamVal qualified as PlLamVal
import LambdaBuffers.Codegen.Plutarch.Print.Refs qualified as PlRefs
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, comma, defaultLayoutOptions, encloseSep, equals, group, hardline, layoutPretty, lparen, parens, pretty, rparen, space, vsep, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Proto.Codegen_Fields qualified as P

hsClassImplPrinters ::
  MonadPrint m =>
  Map
    HsSyntax.QClassName
    ( PC.ModuleName ->
      PC.TyDefs ->
      (Doc ann -> Doc ann) ->
      PC.Ty ->
      m (Doc ann)
    )
hsClassImplPrinters =
  Map.fromList
    [
      ( PlRefs.peqQClassName
      , printDerivePEq
      )
    ,
      ( PlRefs.pisDataQClassName
      , printDerivePIsData
      )
    ,
      ( PlRefs.ptryFromQClassName
      , printDerivePTryFrom
      )
    ,
      ( PlRefs.plutusTypeQClassName
      , printDerivePlutusType
      )
    ]

useVal :: MonadPrint m => HsSyntax.QValName -> m (Doc ann)
useVal qvn = Print.importValue qvn >> return (HsSyntax.printHsQValName qvn)

{- | Deriving PEq.

NOTE(bladyjoker): Doesn't derive the implementation but only uses the underlying PData representation for equality.

```
instance PEq (FooLessTrivial a) where
   (#==) l r = pdata l #== pdata r
```

mkInstanceDoc "\\l r -> (Plutarch.Bool.#==) (Plutarch.Builtin.pdata l) (Plutarch.Builtin.pdata r)"
-}
printDerivePEq :: forall ann m. MonadPrint m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> m (Doc ann)
printDerivePEq _mn _iTyDefs _mkInstanceDoc ty = do
  pdataDoc <- useVal PlRefs.pdataQValName
  peqDoc <- useVal PlRefs.peqQValName
  let implDoc = "\\l r ->" <+> parens peqDoc <+> parens (pdataDoc <+> "l") <+> parens (pdataDoc <+> "r")
  printPEqInstanceDef ty (printValueDef PlRefs.peqMethod implDoc)

printPEqInstanceDef :: MonadPrint m => PC.Ty -> Doc ann -> m (Doc ann)
printPEqInstanceDef ty implDefDoc = do
  Print.importClass PlRefs.peqQClassName
  Print.importClass PlRefs.pisDataQClassName
  let headDoc = HsInstDef.printConstraint PlRefs.peqQClassName ty
      freeVars = HsInstDef.collectTyVars ty
   in case freeVars of
        [] -> return $ "instance" <+> headDoc <+> "where" <> hardline <> space <> space <> implDefDoc
        _ ->
          return $
            "instance"
              <+> HsInstDef.printInstanceContext PlRefs.pisDataQClassName freeVars
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
printDerivePIsData :: forall ann m. MonadPrint m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> m (Doc ann)
printDerivePIsData _mn _iTyDefs mkInstanceDoc _ty = do
  punsafeCoerceDoc <- useVal PlRefs.punsafeCoerceQValName
  let pdataImpl, pfromDataImpl :: Doc ann
      pdataImpl = printValueDef (HsSyntax.MkValueName "pdataImpl") punsafeCoerceDoc
      pfromDataImpl = printValueDef (HsSyntax.MkValueName "pfromDataImpl") punsafeCoerceDoc
  let instanceDoc = mkInstanceDoc (align $ vsep [pdataImpl, pfromDataImpl])
  return instanceDoc

lvPlutusDataBuiltinsForPlutusType :: Map LV.ValueName HsSyntax.QValName
lvPlutusDataBuiltinsForPlutusType =
  Map.fromList
    [ ("toPlutusData", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "toPlutusData"))
    , ("fromPlutusData", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "pfromPlutusDataPlutusType"))
    , ("casePlutusData", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "pcasePlutusData"))
    , ("integerData", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "integerData"))
    , ("constrData", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "constrData"))
    , ("listData", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "listData"))
    , ("succeedParse", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "psucceedParse"))
    , ("failParse", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "pfailParse"))
    , ("bindParse", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "pbindParse"))
    ]

printDerivePlutusType :: MonadPrint m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> m (Doc ann)
printDerivePlutusType mn iTyDefs _mkInstanceDoc ty = do
  pappDoc <- useVal PlRefs.pappQValName
  pconDoc <- useVal PlRefs.pconQValName
  -- HACK(bladyjoker): The `fromData` implementation is trying to construct a term, which for Plutarch means `pcon`. However, this is 'pmatch' implementation which is NOT really exactly 'fromData', and has a different type signature for which we do this. I'm sorry.
  let dirtyHack :: Doc ann -> Doc ann
      dirtyHack = pretty . Text.replace (docToText pconDoc <> " ") "f " . docToText

  let resOrErr =
        do
          toDataE <- deriveToPlutusDataImplPlutarch mn iTyDefs ty
          fromDataE <- deriveFromPlutusDataImplPlutarch mn iTyDefs ty
          (pconImplDoc, imps) <- LV.runPrint lvPlutusDataBuiltinsForPlutusType (HsLamVal.printValueE toDataE)
          (pmatchImplDoc, imps') <- LV.runPrint lvPlutusDataBuiltinsForPlutusType (PlLamVal.printValueE fromDataE)
          let implDoc =
                align $
                  vsep
                    [ printValueDef PlRefs.pconMethod pconImplDoc
                    , printValueDef PlRefs.pmatchMethod $ parens ("\\pd f -> " <+> parens pappDoc <+> parens (dirtyHack pmatchImplDoc) <+> "pd")
                    ]

          return (implDoc, imps' <> imps)
  case resOrErr of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Printing an instance definition for PlutusType failed with: " <> err ^. P.msg)
    Right (implDoc, imps) -> do
      instanceDoc <- printPlutusTypeInstanceDef ty implDoc
      for_ imps Print.importValue
      return instanceDoc
  where
    docToText :: Doc ann -> Text
    docToText = renderStrict . layoutPretty defaultLayoutOptions

printPlutusTypeInstanceDef :: MonadPrint m => PC.Ty -> Doc ann -> m (Doc ann)
printPlutusTypeInstanceDef ty implDefDoc = do
  Print.importClass PlRefs.plutusTypeQClassName
  Print.importClass PlRefs.pisDataQClassName
  Print.importType PlRefs.pdataQTyName
  let headDoc = HsInstDef.printConstraint PlRefs.plutusTypeQClassName ty
      freeVars = HsInstDef.collectTyVars ty
      pinnerDefDoc = "type PInner" <+> HsTyDef.printTyInner ty <+> "=" <+> HsSyntax.printHsQTyName PlRefs.pdataQTyName
   in case freeVars of
        [] ->
          return $
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
          return $
            "instance"
              <+> HsInstDef.printInstanceContext PlRefs.pisDataQClassName freeVars
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

printValueDef :: HsSyntax.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = HsSyntax.printHsValName valName <+> equals <+> valDoc

lvPlutusDataBuiltinsForPTryFrom :: Map LV.ValueName HsSyntax.QValName
lvPlutusDataBuiltinsForPTryFrom =
  Map.fromList
    [ ("toPlutusData", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "toPlutusData"))
    , ("fromPlutusData", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "pfromPlutusDataPTryFrom"))
    , ("casePlutusData", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "pcasePlutusData"))
    , ("integerData", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "integerData"))
    , ("constrData", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "constrData"))
    , ("listData", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "listData"))
    , ("succeedParse", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "psucceedParse"))
    , ("failParse", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "pfailParse"))
    , ("bindParse", (HsSyntax.MkCabalPackageName "lbr-plutarch", HsSyntax.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", HsSyntax.MkValueName "pbindParse"))
    ]

{- | PTryFrom instance implementation.

```haskell
instance (PTryFrom PData (PAsData a)) => PTryFrom PData (PMaybe a) where
  type PTryFromExcess PData (PMaybe a) = Const ()
  ptryFrom' = ptryFromPAsData

instance (PTryFrom PData (PAsData a)) => PTryFrom PData (PAsData (PMaybe a)) where
  type PTryFromExcess PData (PAsData (PMaybe a)) = Const ()
  ptryFrom' pd f = ...
```
-}
printDerivePTryFrom :: MonadPrint m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> Doc ann) -> PC.Ty -> m (Doc ann)
printDerivePTryFrom mn iTyDefs _mkInstanceDoc ty = do
  pappDoc <- useVal PlRefs.pappQValName
  let resOrErr = do
        fromDataE <- deriveFromPlutusDataImplPlutarch mn iTyDefs ty
        (ptryFromImplDoc, imps) <- LV.runPrint lvPlutusDataBuiltinsForPTryFrom (PlLamVal.printValueE fromDataE)
        return
          ( align $ printValueDef PlRefs.ptryFromMethod (parens $ "\\pd f -> f" <+> parens (parens pappDoc <+> parens ptryFromImplDoc <+> "pd" <+> "," <+> "()"))
          , imps
          )
  case resOrErr of
    Left err -> Print.throwInternalError' (mn ^. #sourceInfo) ("Printing an instance definition for PTryFrom failed with: " <> err ^. P.msg)
    Right (implDoc, imps) -> do
      instancePAsDataDoc <- printPTryFromPAsDataInstanceDef ty implDoc
      for_ imps Print.importValue
      instanceDoc <- printPTryFromInstanceDef ty
      return $ align $ vsep [instanceDoc, instancePAsDataDoc]

{- | PTryFrom (PAsData a)

```haskell
instance (PTryFrom PData (PAsData a)) => PTryFrom PData (PAsData (PMaybe a)) where
  type PTryFromExcess PData (PAsData (PMaybe a)) = Const ()
  ptryFrom' pd f = ...
```
-}
printPTryFromPAsDataInstanceDef :: MonadPrint m => PC.Ty -> Doc ann -> m (Doc ann)
printPTryFromPAsDataInstanceDef ty implDefDoc = do
  Print.importClass PlRefs.ptryFromQClassName
  Print.importClass PlRefs.pisDataQClassName
  Print.importType PlRefs.pdataQTyName
  Print.importType PlRefs.pasDataQTyName
  Print.importType PlRefs.constQTyName

  let headDoc =
        HsSyntax.printHsQClassName PlRefs.ptryFromQClassName
          <+> HsSyntax.printHsQTyName PlRefs.pdataQTyName
          <+> parens (HsSyntax.printHsQTyName PlRefs.pasDataQTyName <+> HsTyDef.printTyInner ty)
      freeVars = HsInstDef.collectTyVars ty
      pinnerDefDoc =
        "type PTryFromExcess"
          <+> HsSyntax.printHsQTyName PlRefs.pdataQTyName
          <+> parens (HsSyntax.printHsQTyName PlRefs.pasDataQTyName <+> HsTyDef.printTyInner ty)
          <+> "="
          <+> HsSyntax.printHsQTyName PlRefs.constQTyName
          <+> "()"
   in case freeVars of
        [] ->
          return $
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
          return $
            "instance"
              <+> printContext freeVars
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
  where
    printContext :: [PC.Ty] -> Doc ann
    printContext tys =
      align . group $
        encloseSep
          lparen
          rparen
          comma
          ( [ HsSyntax.printHsQClassName PlRefs.ptryFromQClassName
              <+> HsSyntax.printHsQTyName PlRefs.pdataQTyName
              <+> parens (HsSyntax.printHsQTyName PlRefs.pasDataQTyName <+> HsTyDef.printTyInner t)
            | t <- tys
            ]
              <> [HsSyntax.printConstraint PlRefs.pisDataQClassName t | t <- tys]
          )

{- | PTryFrom instance implementation.

```haskell
instance (PTryFrom PData (PAsData a)) => PTryFrom PData (PMaybe a) where
  type PTryFromExcess PData (PMaybe a) = Const ()
  ptryFrom' = ptryFromPAsData
```
-}
printPTryFromInstanceDef :: MonadPrint m => PC.Ty -> m (Doc ann)
printPTryFromInstanceDef ty = do
  ptryFromPAsDataDoc <- useVal PlRefs.ptryFromPAsDataQValName
  Print.importClass PlRefs.ptryFromQClassName
  Print.importClass PlRefs.pisDataQClassName
  Print.importType PlRefs.pdataQTyName
  Print.importType PlRefs.pasDataQTyName
  Print.importType PlRefs.constQTyName
  let headDoc =
        HsSyntax.printHsQClassName PlRefs.ptryFromQClassName
          <+> HsSyntax.printHsQTyName PlRefs.pdataQTyName
          <+> HsTyDef.printTyInner ty
      freeVars = HsInstDef.collectTyVars ty

      pinnerDefDoc =
        "type PTryFromExcess"
          <+> HsSyntax.printHsQTyName PlRefs.pdataQTyName
          <+> HsTyDef.printTyInner ty
          <+> "="
          <+> HsSyntax.printHsQTyName PlRefs.constQTyName
          <+> "()"

      implDefDoc = printValueDef PlRefs.ptryFromMethod ptryFromPAsDataDoc
   in case freeVars of
        [] ->
          return $
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
          return $
            "instance"
              <+> printContext freeVars
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
  where
    printContext :: [PC.Ty] -> Doc ann
    printContext tys =
      align . group $
        encloseSep
          lparen
          rparen
          comma
          ( [ HsSyntax.printHsQClassName PlRefs.ptryFromQClassName
              <+> HsSyntax.printHsQTyName PlRefs.pdataQTyName
              <+> parens (HsSyntax.printHsQTyName PlRefs.pasDataQTyName <+> HsTyDef.printTyInner t)
            | t <- tys
            ]
              <> [HsSyntax.printConstraint PlRefs.pisDataQClassName t | t <- tys]
          )
