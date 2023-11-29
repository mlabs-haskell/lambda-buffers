{- | `Print.MonadPrint` implementation for the Typescript backend.

 The monad is instantiated with `Ts.QTyName` which are qualified Typescript type
 names referring to `Opaque` type imports. It's also instantiated with
 `[Ts.QClassName]` which denotes the qualified Typescript class names to import.
 Note that a single LambdaBuffers 'class' can be unpacked into several related
 Typescript classes and that's why it's a list of qualified Typescript class names.
-}
module LambdaBuffers.Codegen.Typescript.Print (MonadPrint, printModule) where

import Control.Lens (view, (^.))
import Control.Monad.Reader.Class (ask, asks)
import Control.Monad.State.Class (MonadState (get))
import Data.Foldable (Foldable (toList), foldrM, for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (for)
import LambdaBuffers.Codegen.Config qualified as C
import LambdaBuffers.Codegen.Print (throwInternalError)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Typescript.Print.Derive (printDeriveEq, printDeriveFromPlutusData, printDeriveJson, printDeriveToPlutusData, tsEqClass)
import LambdaBuffers.Codegen.Typescript.Print.InstanceDef (ExportInstanceDecl, printExportInstanceDecl)
import LambdaBuffers.Codegen.Typescript.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Typescript.Print.Names (printModName, printModName', printTyName)
import LambdaBuffers.Codegen.Typescript.Print.TyDef (printTyDef)
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, comma, encloseSep, group, line, lparen, rparen, space, vsep, (<+>))
import Proto.Codegen qualified as P

printModule :: MonadPrint m => m (Doc ann, Set Text)
printModule = do
  ctx <- ask
  tyDefDocs <- printTyDefs (ctx ^. Print.ctxModule)
  instDocs <- printInstances
  st <- get
  let modDoc =
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
      pkgDeps =
        collectPackageDeps
          (ctx ^. Print.ctxTyImports)
          (ctx ^. Print.ctxOpaqueTyImports)
          (ctx ^. Print.ctxClassImports <> st ^. Print.stClassImports)
          (ctx ^. Print.ctxRuleImports)
          (st ^. Print.stValueImports)
  return (modDoc, pkgDeps)

printTyDefs :: MonadPrint m => PC.Module -> m [Doc ann]
printTyDefs m = for (toList $ m ^. #typeDefs) printTyDef

tsClassImplPrinters ::
  Map
    Ts.QClassName
    ( PC.ModuleName ->
      PC.TyDefs ->
      (ExportInstanceDecl (Doc ann)) ->
      PC.Ty ->
      Either P.InternalError (Doc ann, Set Ts.QValName)
    )
tsClassImplPrinters =
  Map.fromList
    [
      ( tsEqClass
      , printDeriveEq
      )
    ,
      ( (Ts.MkPackageName "cardano-transaction-lib", Ts.MkModuleName "Ctl.Internal.ToData", Ts.MkClassName "ToData")
      , printDeriveToPlutusData
      )
    ,
      ( (Ts.MkPackageName "cardano-transaction-lib", Ts.MkModuleName "Ctl.Internal.FromData", Ts.MkClassName "FromData")
      , printDeriveFromPlutusData
      )
    ,
      ( (Ts.MkPackageName "lbr-prelude", Ts.MkModuleName "LambdaBuffers.Runtime.Prelude", Ts.MkClassName "Json")
      , printDeriveJson
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
    Nothing -> throwInternalError (d ^. #constraint . #sourceInfo) ("Missing capability to print " <> show qcn)
    Just pqcns ->
      for
        pqcns
        ( \pqcn -> do
            Print.importClass pqcn
            printTsQClassImpl mn iTyDefs pqcn d
        )

printTsQClassImpl :: MonadPrint m => PC.ModuleName -> PC.TyDefs -> Ts.QClassName -> PC.Derive -> m (Doc ann)
printTsQClassImpl mn iTyDefs hqcn d =
  case Map.lookup hqcn tsClassImplPrinters of
    Nothing -> throwInternalError (d ^. #constraint . #sourceInfo) ("Missing capability to print the Typescript type class " <> show hqcn)
    Just implPrinter -> do
      let ty = d ^. #constraint . #argument
      mkInstanceDoc <- printExportInstanceDecl hqcn ty
      case implPrinter mn iTyDefs mkInstanceDoc ty of
        Left err -> throwInternalError (d ^. #constraint . #sourceInfo) ("Failed printing the implementation for " <> (show hqcn <> "\nGot error: " <> show err))
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

printImports :: Set PC.QTyName -> Set Ts.QTyName -> Set Ts.QClassName -> Set (PC.InfoLess PC.ModuleName) -> Set Ts.QValName -> Doc ann
printImports lbTyImports tsTyImports classImps ruleImps valImps =
  let groupedLbImports =
        Set.fromList [mn | (mn, _tn) <- toList lbTyImports]
          `Set.union` ruleImps
      lbImportDocs = importQualified . printModName' <$> toList groupedLbImports

      groupedTsImports =
        Set.fromList [mn | (_cbl, mn, _tn) <- toList tsTyImports]
          `Set.union` Set.fromList [mn | (_, mn, _) <- toList classImps]
          `Set.union` Set.fromList [mn | (Just (_, mn), _) <- toList valImps]
      tsImportDocs = (\(Ts.MkModuleName mn) -> importQualified $ pretty mn) <$> toList groupedTsImports

      importsDoc = vsep $ lbImportDocs ++ tsImportDocs
   in importsDoc
  where
    importQualified :: Doc ann -> Doc ann
    importQualified mnDoc = "import" <+> mnDoc <+> "as" <+> mnDoc

{- | `collectPackageDeps lbTyImports hsTyImports classImps ruleImps valImps` collects all the package dependencies.
 Note that LB `lbTyImports` and `ruleImps` are wired by the user (as the user decides on the package name for their schemass).
-}
collectPackageDeps :: Set PC.QTyName -> Set Ts.QTyName -> Set Ts.QClassName -> Set (PC.InfoLess PC.ModuleName) -> Set Ts.QValName -> Set Text
collectPackageDeps _lbTyImports hsTyImports classImps _ruleImps valImps =
  let deps =
        Set.fromList [Ts.pkgNameToText pkgName | (pkgName, _, _) <- toList hsTyImports]
          `Set.union` Set.fromList [Ts.pkgNameToText pkgName | (pkgName, _, _) <- toList classImps]
          `Set.union` Set.fromList [Ts.pkgNameToText pkgName | (Just (pkgName, _), _) <- toList valImps]
   in deps
