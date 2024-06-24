{- | `Print.MonadPrint` implementation for the Haskell backend.

 The monad is instantiated with `H.QTyName` which are qualified Haskell type
 names referring to `Opaque` type imports. It's also instantiated with
 `[H.QClassName]` which denotes the qualified Haskell class names to import.
 Note that a single LambdaBuffers 'class' can be unpacked into several related
 Haskell classes and that's why it's a list of qualified Haskell class names.
-}
module LambdaBuffers.Codegen.Haskell.Print (printModule) where

import Control.Lens (view, (^.))
import Control.Monad.Reader.Class (ask, asks)
import Control.Monad.State.Class (MonadState (get))
import Data.Foldable (Foldable (toList), foldrM)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (for)
import LambdaBuffers.Codegen.Config qualified as C
import LambdaBuffers.Codegen.Haskell.Backend (IsHaskellBackend (fromLbModuleName, ghcOptions, languageExtensions, printImplementation, printTyDef), MonadHaskellBackend)
import LambdaBuffers.Codegen.Haskell.Print.InstanceDef (printInstanceDef)
import LambdaBuffers.Codegen.Haskell.Print.Syntax (
  cabalPackageNameToText,
  printTyName,
 )
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as H
import LambdaBuffers.Codegen.Print (throwInternalError)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, comma, encloseSep, group, hsep, line, lparen, rparen, space, vsep, (<+>))

printModule :: forall t m ann. MonadHaskellBackend t m => m (Doc ann, Set Text)
printModule = do
  ctx <- ask
  tyDefDocs <- for (toList $ ctx ^. Print.ctxModule . #typeDefs) (printTyDef @t)
  instDocs <- printInstances
  moduleHeaderDoc <- printModuleHeader (ctx ^. Print.ctxModule . #moduleName) (ctx ^. Print.ctxTyExports)
  importsDoc <- printImports
  let modDoc =
        align . vsep $
          [ printLanguageExtensions (languageExtensions @t)
          , printGhcOptions (ghcOptions @t)
          , moduleHeaderDoc
          , mempty
          , importsDoc
          , mempty
          , vsep ((line <>) <$> tyDefDocs)
          , mempty
          , vsep ((line <>) <$> instDocs)
          ]
  pkgDeps <- collectPackageDeps
  return (modDoc, pkgDeps)

printInstances :: MonadHaskellBackend t m => m [Doc ann]
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

printDerive :: MonadHaskellBackend t m => PC.TyDefs -> PC.Derive -> m [Doc ann]
printDerive iTyDefs d = do
  mn <- asks (view $ Print.ctxModule . #moduleName)
  let qcn = PC.qualifyClassRef mn (d ^. #constraint . #classRef)
  classes <- asks (view $ Print.ctxConfig . C.cfgClasses)
  case Map.lookup qcn classes of
    Nothing -> throwInternalError (d ^. #constraint . #sourceInfo) ("Missing capability to print " <> show qcn) -- TODO(bladyjoker): Fix qcn printing.
    Just hsqcns ->
      for
        hsqcns
        ( \hsqcn -> do
            Print.importClass hsqcn
            printHsQClassImpl mn iTyDefs hsqcn d
        )

printHsQClassImpl :: forall t m ann. MonadHaskellBackend t m => PC.ModuleName -> PC.TyDefs -> H.QClassName -> PC.Derive -> m (Doc ann)
printHsQClassImpl mn iTyDefs hqcn d =
  case Map.lookup hqcn (printImplementation @t) of
    Nothing -> throwInternalError (d ^. #constraint . #sourceInfo) ("Missing capability to print the Haskell type class " <> show hqcn) -- TODO(bladyjoker): Fix hqcn printing
    Just implPrinter -> do
      let ty = d ^. #constraint . #argument
      mkInstanceDoc <- printInstanceDef hqcn ty
      implPrinter mn iTyDefs mkInstanceDoc ty

printLanguageExtensions :: Pretty a => [a] -> Doc ann
printLanguageExtensions [] = mempty
printLanguageExtensions exts = "{-# LANGUAGE" <+> align (encloseSep mempty mempty comma (pretty <$> exts)) <+> "#-}"

printGhcOptions :: Pretty a => [a] -> Doc ann
printGhcOptions [] = mempty
printGhcOptions opts = "{-# OPTIONS_GHC" <+> align (hsep (pretty <$> opts)) <+> "#-}"

printModuleHeader :: forall t m ann. MonadHaskellBackend t m => PC.ModuleName -> Set (PC.InfoLess PC.TyName) -> m (Doc ann)
printModuleHeader mn exports = return $ "module" <+> H.printModName (fromLbModuleName @t) mn <+> printExports exports <+> "where"

printExports :: Set (PC.InfoLess PC.TyName) -> Doc ann
printExports exports = align $ group $ encloseSep lparen rparen (comma <> space) ((`PC.withInfoLess` printTyExportWithCtors) <$> toList exports)
  where
    printTyExportWithCtors :: PC.TyName -> Doc ann
    printTyExportWithCtors tyn = printTyName tyn <> "(..)"

printImports :: forall t m ann. MonadHaskellBackend t m => m (Doc ann)
printImports = do
  ctx <- ask
  st <- get
  let lbTyImports = ctx ^. Print.ctxTyImports
      hsTyImports = ctx ^. Print.ctxOpaqueTyImports <> st ^. Print.stTypeImports
      classImps = ctx ^. Print.ctxClassImports <> st ^. Print.stClassImports
      ruleImps = ctx ^. Print.ctxRuleImports
      valImps = st ^. Print.stValueImports

  let groupedLbImports =
        Set.fromList [mn | (mn, _tn) <- toList lbTyImports]
          `Set.union` ruleImps
      lbImportDocs = importQualified . H.printModName (fromLbModuleName @t) . (`PC.withInfoLess` id) <$> toList groupedLbImports

      groupedHsImports =
        Set.fromList [mn | (_cbl, mn, _tn) <- toList hsTyImports]
          `Set.union` Set.fromList [mn | (_, mn, _) <- toList classImps]
          `Set.union` Set.fromList [mn | (_, mn, _) <- toList valImps]
      hsImportDocs = (\(H.MkModuleName mn) -> importQualified $ pretty mn) <$> toList groupedHsImports

      importsDoc = vsep $ lbImportDocs ++ hsImportDocs

  return importsDoc
  where
    importQualified :: Doc ann -> Doc ann
    importQualified mnDoc = "import qualified" <+> mnDoc

{- | `collectPackageDeps` collects all the package dependencies.
 Note that LB `lbTyImports` and `ruleImps` are wired by the user (as the user decides on the package name for their schemass).
-}
collectPackageDeps :: forall t m. MonadHaskellBackend t m => m (Set Text)
collectPackageDeps = do
  ctx <- ask
  st <- get
  let hsTyImports = ctx ^. Print.ctxOpaqueTyImports <> st ^. Print.stTypeImports
      classImps = ctx ^. Print.ctxClassImports <> st ^. Print.stClassImports
      valImps = st ^. Print.stValueImports

      deps =
        Set.singleton "base"
          `Set.union` Set.fromList [cabalPackageNameToText cbl | (cbl, _, _) <- toList hsTyImports]
          `Set.union` Set.fromList [cabalPackageNameToText cbl | (cbl, _, _) <- toList classImps]
          `Set.union` Set.fromList [cabalPackageNameToText cbl | (cbl, _, _) <- toList valImps]

  return deps
