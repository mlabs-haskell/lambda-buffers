module LambdaBuffers.Codegen.Haskell.Backend.Plutarch.Derive (hsClassImplPrinters) where

import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Backend (MonadHaskellBackend)
import LambdaBuffers.Codegen.Haskell.Backend.Plutarch.LamVal qualified as PlLamVal
import LambdaBuffers.Codegen.Haskell.Backend.Plutarch.Refs qualified as PlRefs
import LambdaBuffers.Codegen.Haskell.Print.InstanceDef qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Print.TyDef qualified as Haskell
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import LambdaBuffers.Codegen.LamVal.PlutusData (deriveFromPlutusDataImplPlutarch, deriveToPlutusDataImplPlutarch)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, comma, defaultLayoutOptions, encloseSep, equals, group, hardline, layoutPretty, lparen, parens, pretty, rparen, space, vsep, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Proto.Codegen_Fields qualified as P

hsClassImplPrinters ::
  MonadHaskellBackend t m =>
  Map
    Haskell.QClassName
    ( PC.ModuleName ->
      PC.TyDefs ->
      (Doc ann -> m (Doc ann)) ->
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

useVal :: MonadHaskellBackend t m => Haskell.QValName -> m (Doc ann)
useVal qvn = Print.importValue qvn >> return (Haskell.printHsQValName qvn)

printValue :: (LV.Ref -> Maybe Haskell.QValName) -> LV.ValueE -> Either LV.PrintError (Doc ann, Set Haskell.QValName)
printValue builtins valE = LV.runPrint (LV.Context builtins ()) (PlLamVal.printValueE valE)

{- | Deriving PEq.

NOTE(bladyjoker): Doesn't derive the implementation but only uses the underlying PData representation for equality.

```
instance PEq (FooLessTrivial a) where
   (#==) l r = pdata l #== pdata r
```

mkInstanceDoc "\\l r -> (Plutarch.Bool.#==) (Plutarch.Builtin.pdata l) (Plutarch.Builtin.pdata r)"
-}
printDerivePEq :: forall t m ann. MonadHaskellBackend t m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> m (Doc ann)) -> PC.Ty -> m (Doc ann)
printDerivePEq _mn _iTyDefs _mkInstanceDoc ty = do
  pdataDoc <- useVal PlRefs.pdataQValName
  peqDoc <- useVal PlRefs.peqQValName
  let implDoc = "\\l r ->" <+> parens peqDoc <+> parens (pdataDoc <+> "l") <+> parens (pdataDoc <+> "r")
  printPEqInstanceDef ty (printValueDef PlRefs.peqMethod implDoc)

printPEqInstanceDef :: MonadHaskellBackend t m => PC.Ty -> Doc ann -> m (Doc ann)
printPEqInstanceDef ty implDefDoc = do
  Print.importClass PlRefs.peqQClassName
  Print.importClass PlRefs.pisDataQClassName
  let freeVars = Haskell.collectTyVars ty
  headDoc <- Haskell.printConstraint PlRefs.peqQClassName ty
  case freeVars of
    [] -> return $ "instance" <+> headDoc <+> "where" <> hardline <> space <> space <> implDefDoc
    _other -> do
      instanceCtxDoc <- Haskell.printInstanceContext PlRefs.pisDataQClassName freeVars
      return $
        "instance"
          <+> instanceCtxDoc
          <+> "=>"
          <+> headDoc
          <+> "where"
          <> hardline
          <> space
          <> space
          <> implDefDoc

{- | Deriving PIsData.

NOTE(bladyjoker): Doesn't derive the implementation but only uses `punsafeCoerce`.

```
instance PIsData (FooLessTrivial a) where
  pdataImpl = punsafeCoerce
  pfromDataImpl = punsafeCoerce
```
-}
printDerivePIsData :: forall t m ann. MonadHaskellBackend t m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> m (Doc ann)) -> PC.Ty -> m (Doc ann)
printDerivePIsData _mn _iTyDefs mkInstanceDoc _ty = do
  punsafeCoerceDoc <- useVal PlRefs.punsafeCoerceQValName
  let pdataImpl, pfromDataImpl :: Doc ann
      pdataImpl = printValueDef (Haskell.MkValueName "pdataImpl") punsafeCoerceDoc
      pfromDataImpl = printValueDef (Haskell.MkValueName "pfromDataImpl") punsafeCoerceDoc
  mkInstanceDoc (align $ vsep [pdataImpl, pfromDataImpl])

lvPlutusDataBuiltinsForPlutusType :: LV.Ref -> Maybe Haskell.QValName
lvPlutusDataBuiltinsForPlutusType (_ty, refName) =
  Map.lookup refName $
    Map.fromList
      [ ("toPlutusData", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "toPlutusData"))
      , ("fromPlutusData", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "pfromPlutusDataPlutusType"))
      , ("casePlutusData", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "pcasePlutusData"))
      , ("integerData", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "integerData"))
      , ("constrData", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "constrData"))
      , ("listData", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "listData"))
      , ("succeedParse", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "psucceedParse"))
      , ("failParse", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "pfailParse"))
      , ("bindParse", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "pbindParse"))
      ]

printDerivePlutusType :: MonadHaskellBackend t m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> m (Doc ann)) -> PC.Ty -> m (Doc ann)
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
          (pconImplDoc, imps) <- printValue lvPlutusDataBuiltinsForPlutusType toDataE
          (pmatchImplDoc, imps') <- printValue lvPlutusDataBuiltinsForPlutusType fromDataE
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

printPlutusTypeInstanceDef :: MonadHaskellBackend t m => PC.Ty -> Doc ann -> m (Doc ann)
printPlutusTypeInstanceDef ty implDefDoc = do
  Print.importClass PlRefs.plutusTypeQClassName
  Print.importClass PlRefs.pisDataQClassName
  Print.importType PlRefs.pdataQTyName
  headDoc <- Haskell.printConstraint PlRefs.plutusTypeQClassName ty
  tyDoc <- Haskell.printTyInner ty
  let freeVars = Haskell.collectTyVars ty
      pinnerDefDoc = "type PInner" <+> tyDoc <+> "=" <+> Haskell.printHsQTyName PlRefs.pdataQTyName
  case freeVars of
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
    _other -> do
      instanceCtxDoc <- Haskell.printInstanceContext PlRefs.pisDataQClassName freeVars
      return $
        "instance"
          <+> instanceCtxDoc
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

printValueDef :: Haskell.ValueName -> Doc ann -> Doc ann
printValueDef valName valDoc = Haskell.printHsValName valName <+> equals <+> valDoc

lvPlutusDataBuiltinsForPTryFrom :: LV.Ref -> Maybe Haskell.QValName
lvPlutusDataBuiltinsForPTryFrom (_ty, refName) =
  Map.lookup refName $
    Map.fromList
      [ ("toPlutusData", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "toPlutusData"))
      , ("fromPlutusData", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "pfromPlutusDataPTryFrom"))
      , ("casePlutusData", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "pcasePlutusData"))
      , ("integerData", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "integerData"))
      , ("constrData", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "constrData"))
      , ("listData", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "listData"))
      , ("succeedParse", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "psucceedParse"))
      , ("failParse", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "pfailParse"))
      , ("bindParse", (Haskell.MkCabalPackageName "lbr-plutarch", Haskell.MkModuleName "LambdaBuffers.Runtime.Plutarch.LamVal", Haskell.MkValueName "pbindParse"))
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
printDerivePTryFrom :: MonadHaskellBackend t m => PC.ModuleName -> PC.TyDefs -> (Doc ann -> m (Doc ann)) -> PC.Ty -> m (Doc ann)
printDerivePTryFrom mn iTyDefs _mkInstanceDoc ty = do
  pappDoc <- useVal PlRefs.pappQValName
  let resOrErr = do
        fromDataE <- deriveFromPlutusDataImplPlutarch mn iTyDefs ty
        (ptryFromImplDoc, imps) <- printValue lvPlutusDataBuiltinsForPTryFrom fromDataE
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
printPTryFromPAsDataInstanceDef :: MonadHaskellBackend t m => PC.Ty -> Doc ann -> m (Doc ann)
printPTryFromPAsDataInstanceDef ty implDefDoc = do
  Print.importClass PlRefs.ptryFromQClassName
  Print.importClass PlRefs.pisDataQClassName
  Print.importType PlRefs.pdataQTyName
  Print.importType PlRefs.pasDataQTyName
  Print.importType PlRefs.constQTyName

  tyDoc <- Haskell.printTyInner ty
  let headDoc =
        Haskell.printHsQClassName PlRefs.ptryFromQClassName
          <+> Haskell.printHsQTyName PlRefs.pdataQTyName
          <+> parens (Haskell.printHsQTyName PlRefs.pasDataQTyName <+> tyDoc)
      freeVars = Haskell.collectTyVars ty
      pinnerDefDoc =
        "type PTryFromExcess"
          <+> Haskell.printHsQTyName PlRefs.pdataQTyName
          <+> parens (Haskell.printHsQTyName PlRefs.pasDataQTyName <+> tyDoc)
          <+> "="
          <+> Haskell.printHsQTyName PlRefs.constQTyName
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
        _other -> do
          ctxDoc <- printContext freeVars
          return $
            "instance"
              <+> ctxDoc
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

{- | PTryFrom instance implementation.

```haskell
instance (PTryFrom PData (PAsData a)) => PTryFrom PData (PMaybe a) where
  type PTryFromExcess PData (PMaybe a) = Const ()
  ptryFrom' = ptryFromPAsData
```
-}
printPTryFromInstanceDef :: MonadHaskellBackend t m => PC.Ty -> m (Doc ann)
printPTryFromInstanceDef ty = do
  ptryFromPAsDataDoc <- useVal PlRefs.ptryFromPAsDataQValName
  Print.importClass PlRefs.ptryFromQClassName
  Print.importClass PlRefs.pisDataQClassName
  Print.importType PlRefs.pdataQTyName
  Print.importType PlRefs.pasDataQTyName
  Print.importType PlRefs.constQTyName
  tyDoc <- Haskell.printTyInner ty
  let headDoc =
        Haskell.printHsQClassName PlRefs.ptryFromQClassName
          <+> Haskell.printHsQTyName PlRefs.pdataQTyName
          <+> tyDoc
      freeVars = Haskell.collectTyVars ty

      pinnerDefDoc =
        "type PTryFromExcess"
          <+> Haskell.printHsQTyName PlRefs.pdataQTyName
          <+> tyDoc
          <+> "="
          <+> Haskell.printHsQTyName PlRefs.constQTyName
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
        _other -> do
          ctxDoc <- printContext freeVars
          return $
            "instance"
              <+> ctxDoc
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

printContext :: MonadHaskellBackend t m => [PC.Ty] -> m (Doc ann)
printContext tys = do
  ctxDocs <-
    traverse
      ( \t -> do
          tyDoc <- Haskell.printTyInner t
          return $
            Haskell.printHsQClassName PlRefs.ptryFromQClassName
              <+> Haskell.printHsQTyName PlRefs.pdataQTyName
              <+> parens (Haskell.printHsQTyName PlRefs.pasDataQTyName <+> tyDoc)
      )
      tys
  ctxDocs' <- traverse (Haskell.printConstraint PlRefs.pisDataQClassName) tys
  return $
    align . group $
      encloseSep
        lparen
        rparen
        comma
        (ctxDocs <> ctxDocs')
