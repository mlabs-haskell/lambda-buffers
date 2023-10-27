{- | `Print.MonadPrint` implementation for the Haskell backend.

 The monad is instantiated with `H.QTyName` which are qualified Haskell type
 names referring to `Opaque` type imports. It's also instantiated with
 `[H.QClassName]` which denotes the qualified Haskell class names to import.
 Note that a single LambdaBuffers 'class' can be unpacked into several related
 Haskell classes and that's why it's a list of qualified Haskell class names.
-}
module LambdaBuffers.Codegen.Haskell.Print (MonadPrint, printModule, PrintModuleEnv (..)) where

import Control.Lens (view, (^.))
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
import LambdaBuffers.Codegen.Haskell.Print.InstanceDef (printInstanceDef)
import LambdaBuffers.Codegen.Haskell.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Haskell.Print.Syntax (
  cabalPackageNameToText,
  printModName',
  printTyName,
 )
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as H
import LambdaBuffers.Codegen.Haskell.Print.TyDef (printTyDef)
import LambdaBuffers.Codegen.Print (throwInternalError)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, comma, encloseSep, group, line, lparen, rparen, space, vsep, (<+>))
import Proto.Codegen qualified as P
import Proto.Codegen_Fields qualified as P

data PrintModuleEnv ann = PrintModuleEnv
  { env'printModuleName :: PC.ModuleName -> Doc ann
  , env'implementationPrinter ::
      Map
        H.QClassName
        ( PC.ModuleName ->
          PC.TyDefs ->
          (Doc ann -> Doc ann) ->
          PC.Ty ->
          Either P.InternalError (Doc ann, Set H.QValName)
        )
  }

printModule :: MonadPrint m => PrintModuleEnv ann -> m (Doc ann, Set Text)
printModule env = do
  ctx <- ask
  tyDefDocs <- for (toList $ ctx ^. Print.ctxModule . #typeDefs) printTyDef
  instDocs <- printInstances env
  st <- get
  let modDoc =
        align . vsep $
          [ printModuleHeader env (ctx ^. Print.ctxModule . #moduleName) (ctx ^. Print.ctxTyExports)
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

printInstances :: MonadPrint m => PrintModuleEnv ann -> m [Doc ann]
printInstances env = do
  ci <- asks (view Print.ctxCompilerInput)
  m <- asks (view Print.ctxModule)
  let iTyDefs = PC.indexTyDefs ci
  foldrM
    ( \d instDocs -> do
        instDocs' <- printDerive env iTyDefs d
        return $ instDocs' <> instDocs
    )
    mempty
    (toList $ m ^. #derives)

printDerive :: MonadPrint m => PrintModuleEnv ann -> PC.TyDefs -> PC.Derive -> m [Doc ann]
printDerive env iTyDefs d = do
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
            printHsQClassImpl env mn iTyDefs hsqcn d
        )

printHsQClassImpl :: MonadPrint m => PrintModuleEnv ann -> PC.ModuleName -> PC.TyDefs -> H.QClassName -> PC.Derive -> m (Doc ann)
printHsQClassImpl env mn iTyDefs hqcn d =
  case Map.lookup hqcn (env'implementationPrinter env) of
    Nothing -> throwInternalError (d ^. #constraint . #sourceInfo) ("Missing capability to print the Haskell type class " <> show hqcn) -- TODO(bladyjoker): Fix hqcn printing
    Just implPrinter -> do
      let ty = d ^. #constraint . #argument
          mkInstanceDoc = printInstanceDef hqcn ty
      case implPrinter mn iTyDefs mkInstanceDoc ty of
        Left err ->
          throwInternalError
            (d ^. #constraint . #sourceInfo)
            ("Failed printing the implementation for " <> show hqcn <> "\nGot error: " <> Text.unpack (err ^. P.msg))
        Right (instanceDefsDoc, valImps) -> do
          for_ (toList valImps) Print.importValue
          return instanceDefsDoc

printModuleHeader :: PrintModuleEnv ann -> PC.ModuleName -> Set (PC.InfoLess PC.TyName) -> Doc ann
printModuleHeader env mn exports = "module" <+> env'printModuleName env mn <+> printExports exports <+> "where"

printExports :: Set (PC.InfoLess PC.TyName) -> Doc ann
printExports exports = align $ group $ encloseSep lparen rparen (comma <> space) ((`PC.withInfoLess` printTyExportWithCtors) <$> toList exports)
  where
    printTyExportWithCtors :: PC.TyName -> Doc ann
    printTyExportWithCtors tyn = printTyName tyn <> "(..)"

printImports :: Set PC.QTyName -> Set H.QTyName -> Set H.QClassName -> Set (PC.InfoLess PC.ModuleName) -> Set H.QValName -> Doc ann
printImports lbTyImports hsTyImports classImps ruleImps valImps =
  let groupedLbImports =
        Set.fromList [mn | (mn, _tn) <- toList lbTyImports]
          `Set.union` ruleImps
      lbImportDocs = importQualified . printModName' <$> toList groupedLbImports

      groupedHsImports =
        Set.fromList [mn | (_cbl, mn, _tn) <- toList hsTyImports]
          `Set.union` Set.fromList [mn | (_, mn, _) <- toList classImps]
          `Set.union` Set.fromList [mn | (_, mn, _) <- toList valImps]
      hsImportDocs = (\(H.MkModuleName mn) -> importQualified $ pretty mn) <$> toList groupedHsImports

      importsDoc = vsep $ lbImportDocs ++ hsImportDocs
   in importsDoc
  where
    importQualified :: Doc ann -> Doc ann
    importQualified mnDoc = "import qualified" <+> mnDoc

{- | `collectPackageDeps lbTyImports hsTyImports classImps ruleImps valImps` collects all the package dependencies.
 Note that LB `lbTyImports` and `ruleImps` are wired by the user (as the user decides on the package name for their schemass).
-}
collectPackageDeps :: Set PC.QTyName -> Set H.QTyName -> Set H.QClassName -> Set (PC.InfoLess PC.ModuleName) -> Set H.QValName -> Set Text
collectPackageDeps _lbTyImports hsTyImports classImps _ruleImps valImps =
  let deps =
        Set.singleton "base"
          `Set.union` Set.fromList [cabalPackageNameToText cbl | (cbl, _, _) <- toList hsTyImports]
          `Set.union` Set.fromList [cabalPackageNameToText cbl | (cbl, _, _) <- toList classImps]
          `Set.union` Set.fromList [cabalPackageNameToText cbl | (cbl, _, _) <- toList valImps]
   in deps
