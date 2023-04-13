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
import Control.Monad.State.Class (MonadState (get))
import Data.Foldable (Foldable (toList), foldrM, for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Codegen.Config qualified as C
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Purescript.Print.Derive (printDeriveEq, printDeriveFromPlutusData, printDeriveToPlutusData)
import LambdaBuffers.Codegen.Purescript.Print.InstanceDef (printInstanceDef)
import LambdaBuffers.Codegen.Purescript.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Purescript.Print.Names (printModName, printModName', printTyName)
import LambdaBuffers.Codegen.Purescript.Print.TyDef (printTyDef)
import LambdaBuffers.Codegen.Purescript.Syntax qualified as Purs
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, comma, encloseSep, group, line, lparen, rparen, space, vsep, (<+>))

printModule :: MonadPrint m => m (Doc ann)
printModule = do
  ctx <- ask
  tyDefDocs <- printTyDefs (ctx ^. Print.ctxModule)
  instDocs <- printInstances
  st <- get
  return $
    align . vsep $
      [ printModuleHeader (ctx ^. Print.ctxModule . #moduleName) (ctx ^. Print.ctxTyExports)
      , mempty
      , printImports
          (ctx ^. Print.ctxTyImports)
          (ctx ^. Print.ctxOpaqueTyImports)
          (ctx ^. Print.ctxClassImports <> st ^. Print.stClassImports)
          (ctx ^. Print.ctxRuleImports)
          (st ^. Print.stValueImports)
      , mempty
      , vsep ((line <>) <$> tyDefDocs)
      , mempty
      , vsep ((line <>) <$> instDocs)
      ]

printTyDefs :: MonadPrint m => PC.Module -> m [Doc ann]
printTyDefs m = for (toList $ m ^. #typeDefs) printTyDef

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
    ,
      ( (Purs.MkPackageName "cardano-transaction-lib", Purs.MkModuleName "Ctl.Internal.FromData", Purs.MkClassName "FromData")
      , printDeriveFromPlutusData
      )
    ]

printInstances :: MonadPrint m => m [Doc ann]
printInstances = do
  ci <- asks (view Print.ctxCompilerInput)
  m <- asks (view Print.ctxModule)
  let iTyDefs = PC.indexTyDefs ci
  foldrM
    ( \d instDocs -> do
        instDocs' <- printDerive iTyDefs d
        return $ instDocs' <> instDocs
    )
    mempty
    (toList $ m ^. #derives)

printDerive :: MonadPrint m => PC.TyDefs -> PC.Derive -> m [Doc ann]
printDerive iTyDefs d = do
  mn <- asks (view $ Print.ctxModule . #moduleName)
  let qcn = PC.qualifyClassRef mn (d ^. #constraint . #classRef)
  classes <- asks (view $ Print.ctxConfig . C.cfgClasses)
  case Map.lookup qcn classes of
    Nothing -> throwError (d ^. #constraint . #sourceInfo, "TODO(bladyjoker): Missing capability to print " <> (Text.pack . show $ qcn))
    Just pqcns -> for pqcns (\pqcn -> printPursQClassImpl mn iTyDefs pqcn d)

printPursQClassImpl :: MonadPrint m => PC.ModuleName -> PC.TyDefs -> Purs.QClassName -> PC.Derive -> m (Doc ann)
printPursQClassImpl mn iTyDefs hqcn d =
  case Map.lookup hqcn pursClassImplPrinters of
    Nothing -> throwError (d ^. #constraint . #sourceInfo, "TODO(bladyjoker): Missing capability to print the Purescript type class " <> (Text.pack . show $ hqcn))
    Just implPrinter -> do
      let ty = d ^. #constraint . #argument
          mkInstanceDoc = printInstanceDef hqcn ty
      case implPrinter mn iTyDefs mkInstanceDoc ty of
        Left err -> throwError (d ^. #constraint . #sourceInfo, "Failed printing the implementation for " <> (Text.pack . show $ hqcn) <> "\nGot error: " <> err)
        Right (instanceDefsDoc, valImps) -> do
          for_ (toList valImps) Print.importValue
          return instanceDefsDoc

printModuleHeader :: PC.ModuleName -> Set (PC.InfoLess PC.TyName) -> Doc ann
printModuleHeader mn exports = "module" <+> printModName mn <+> printExports exports <+> "where"

printExports :: Set (PC.InfoLess PC.TyName) -> Doc ann
printExports exports = align $ group $ encloseSep lparen rparen (comma <> space) ((`PC.withInfoLess` printTyExportWithCtors) <$> toList exports)
  where
    printTyExportWithCtors :: PC.TyName -> Doc ann
    printTyExportWithCtors tyn = printTyName tyn <> "(..)"

-- TODO(bladyjoker): Collect package dependencies.
printImports :: Set PC.QTyName -> Set Purs.QTyName -> Set Purs.QClassName -> Set (PC.InfoLess PC.ModuleName) -> Set Purs.QValName -> Doc ann
printImports lbTyImports pursTyImports classImps ruleImps valImps =
  let groupedLbImports =
        Set.fromList [mn | (mn, _tn) <- toList lbTyImports]
          `Set.union` ruleImps
      lbImportDocs = importQualified . printModName' <$> toList groupedLbImports

      groupedPursImports =
        Set.fromList [mn | (_cbl, mn, _tn) <- toList pursTyImports]
          `Set.union` Set.fromList [mn | (_, mn, _) <- toList classImps]
          `Set.union` Set.fromList [mn | (Just (_, mn), _) <- toList valImps]
      pursImportDocs = (\(Purs.MkModuleName mn) -> importQualified $ pretty mn) <$> toList groupedPursImports

      importsDoc = vsep $ lbImportDocs ++ pursImportDocs
   in importsDoc
  where
    importQualified :: Doc ann -> Doc ann
    importQualified mnDoc = "import" <+> mnDoc <+> "as" <+> mnDoc