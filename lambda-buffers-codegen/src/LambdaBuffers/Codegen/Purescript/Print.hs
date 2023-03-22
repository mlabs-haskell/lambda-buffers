{- | `Print.MonadPrint` implementation for the Purescript backend.

 The monad is instantiated with `Purs.QTyName` which are qualified Purescript type
 names referring to `Opaque` type imports. It's also instantiated with
 `[Purs.QClassName]` which denotes the qualified Purescript class names to import.
 Note that a single LambdaBuffers 'class' can be unpacked into several related
 Purescript classes and that's why it's a list of qualified Purescript class names.
-}
module LambdaBuffers.Codegen.Purescript.Print (MonadPrint, printModule) where

import Control.Lens (view, (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Reader.Class (ask, asks)
import Data.Foldable (Foldable (toList), foldrM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Codegen.Config qualified as C
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Purescript.Print.Derive (printDeriveEq, printDeriveToPlutusData)
import LambdaBuffers.Codegen.Purescript.Print.InstanceDef (printInstanceDef)
import LambdaBuffers.Codegen.Purescript.Print.Monad (MonadPrint)
import LambdaBuffers.Codegen.Purescript.Print.Names (printModName, printModName', printTyName)
import LambdaBuffers.Codegen.Purescript.Print.TyDef (printTyDef)
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purs
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, comma, encloseSep, group, lparen, rparen, space, vsep, (<+>))

printModule :: MonadPrint m => m (Doc ann)
printModule = do
  Print.MkContext _ci m lbTyImps opTyImps classImps ruleImps tyExps _cfg <- ask
  tyDefDocs <- for (toList $ m ^. #typeDefs) printTyDef
  (instDocs, valImps) <- printInstances
  return $
    align . vsep $
      [ printModuleHeader (m ^. #moduleName) tyExps
      , mempty
      , printImports lbTyImps opTyImps classImps ruleImps valImps
      , mempty
      , vsep tyDefDocs -- TODO(bladyjoker): Add additional line in between TyDefs.
      , mempty
      , vsep instDocs -- TODO(bladyjoker): Add additional line in between implementations.
      ]

pursClassImplPrinters ::
  Map
    Purs.QClassName
    ( PC.ModuleName ->
      PC.TyDefs ->
      (Doc ann -> Doc ann) ->
      PC.Ty ->
      Either Text (Doc ann, Set Purs.QValName)
    )
pursClassImplPrinters =
  Map.fromList
    [
      ( (Purs.MkPackageName "prelude", Purs.MkModuleName "Prelude", Purs.MkClassName "Eq")
      , printDeriveEq
      )
    ,
      ( (Purs.MkPackageName "cardano-transaction-lib", Purs.MkModuleName "Ctl.Internal.ToData", Purs.MkClassName "ToData")
      , printDeriveToPlutusData
      )
    ]

printInstances :: MonadPrint m => m ([Doc ann], Set Purs.QValName)
printInstances = do
  ci <- asks (view Print.ctxCompilerInput)
  m <- asks (view Print.ctxModule)
  let iTyDefs = PC.indexTyDefs ci
  foldrM
    ( \d (instDocs, valImps) -> do
        (instDoc', valImps') <- printDerive iTyDefs d
        return (instDoc' : instDocs, valImps `Set.union` valImps')
    )
    (mempty, Set.empty)
    (toList $ m ^. #derives)

printDerive :: MonadPrint m => PC.TyDefs -> PC.Derive -> m (Doc ann, Set Purs.QValName)
printDerive iTyDefs d = do
  mn <- asks (view $ Print.ctxModule . #moduleName)
  let qcn = PC.qualifyClassRef mn (d ^. #constraint . #classRef)
  classes <- asks (view $ Print.ctxConfig . C.classes)
  case Map.lookup qcn classes of
    Nothing -> throwError (d ^. #constraint . #sourceInfo, "TODO(bladyjoker): Missing capability to print " <> (Text.pack . show $ qcn))
    Just pursQClassNamesToPrint -> do
      res <- for pursQClassNamesToPrint (\pursqcn -> printPursQClassImpl mn iTyDefs pursqcn d)
      return (vsep (fst <$> res), Set.unions (snd <$> res))

printPursQClassImpl :: MonadPrint m => PC.ModuleName -> PC.TyDefs -> Purs.QClassName -> PC.Derive -> m (Doc ann, Set Purs.QValName)
printPursQClassImpl mn iTyDefs hqcn d =
  case Map.lookup hqcn pursClassImplPrinters of
    Nothing -> throwError (d ^. #constraint . #sourceInfo, "TODO(bladyjoker): Missing capability to print the Purescript type class " <> (Text.pack . show $ hqcn))
    Just implPrinter -> do
      let ty = d ^. #constraint . #argument
          mkInstanceDoc = printInstanceDef hqcn ty
      case implPrinter mn iTyDefs mkInstanceDoc ty of
        Left err -> throwError (d ^. #constraint . #sourceInfo, "Failed printing the implementation for " <> (Text.pack . show $ hqcn) <> "\nGot error: " <> err)
        Right (instanceDefsDoc, valImps) -> return (instanceDefsDoc, valImps)

printModuleHeader :: PC.ModuleName -> Set (PC.InfoLess PC.TyName) -> Doc ann
printModuleHeader mn exports = "module" <+> printModName mn <+> printExports exports <+> "where"

printExports :: Set (PC.InfoLess PC.TyName) -> Doc ann
printExports exports = align $ group $ encloseSep lparen rparen (comma <> space) ((`PC.withInfoLess` printTyExportWithCtors) <$> toList exports)
  where
    printTyExportWithCtors :: PC.TyName -> Doc ann
    printTyExportWithCtors tyn = printTyName tyn <> "(..)"

escapeHatchImports :: [(Purs.PackageName, Purs.ModuleName)]
escapeHatchImports =
  [ (Purs.MkPackageName "newtype", Purs.MkModuleName "Data.Newtype")
  , (Purs.MkPackageName "bigints", Purs.MkModuleName "Data.BigInt")
  ]

-- TODO(bladyjoker): Collect package dependencies.
printImports :: Set PC.QTyName -> Set Purs.QTyName -> Set [Purs.QClassName] -> Set (PC.InfoLess PC.ModuleName) -> Set Purs.QValName -> Doc ann
printImports lbTyImports pursTyImports classImps ruleImps valImps =
  let groupedLbImports =
        Set.fromList [mn | (mn, _tn) <- toList lbTyImports]
          `Set.union` ruleImps
      lbImportDocs = importQualified . printModName' <$> toList groupedLbImports

      groupedPursImports =
        Set.fromList [mn | (_cbl, mn, _tn) <- toList pursTyImports]
          `Set.union` Set.fromList [mn | cImps <- toList classImps, (_, mn, _) <- cImps]
          `Set.union` Set.fromList [mn | (Just (_, mn), _) <- toList valImps]
          `Set.union` Set.fromList [mn | (_, mn) <- escapeHatchImports]
      pursImportDocs = (\(Purs.MkModuleName mn) -> importQualified $ pretty mn) <$> toList groupedPursImports

      importsDoc = vsep $ lbImportDocs ++ pursImportDocs
   in importsDoc
  where
    importQualified :: Doc ann -> Doc ann
    importQualified mnDoc = "import" <+> mnDoc <+> "as" <+> mnDoc