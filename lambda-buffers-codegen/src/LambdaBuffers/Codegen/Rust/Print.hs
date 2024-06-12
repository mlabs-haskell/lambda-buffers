module LambdaBuffers.Codegen.Rust.Print (printModule) where

import Control.Lens (view, (^.))
import Control.Monad.Reader.Class (ask, asks)
import Control.Monad.State.Class (MonadState (get))
import Data.Foldable (Foldable (toList), foldrM)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (for)
import LambdaBuffers.Codegen.Config qualified as C
import LambdaBuffers.Codegen.Print (throwInternalError)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Rust.Backend (MonadRustBackend, RustBackendContext (rust'compilationCfgs, rust'packages))
import LambdaBuffers.Codegen.Rust.Print.Derive qualified as Rust
import LambdaBuffers.Codegen.Rust.Print.InstanceDef (printInstanceDef)
import LambdaBuffers.Codegen.Rust.Print.Syntax (crateNameToCargoText, crateNameToText)
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R
import LambdaBuffers.Codegen.Rust.Print.TyDef qualified as Rust
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, brackets, line, semi, vsep, (<+>))

printModule :: MonadRustBackend m => m (Doc ann, Set Text)
printModule = do
  ctx <- ask
  let rustCtx = ctx ^. Print.ctxBackend
  tyDefDocs <- for (toList $ ctx ^. Print.ctxModule . #typeDefs) Rust.printTyDef
  instDocs <- printInstances
  let compCfgDoc = printCompilationCfgs (rust'compilationCfgs rustCtx)
  imports <- collectPackageDeps
  let importsDoc = printImports imports
  let modDoc =
        align . vsep $
          [ compCfgDoc
          , importsDoc
          , mempty
          , vsep ((line <>) <$> tyDefDocs)
          , mempty
          , vsep ((line <>) <$> instDocs)
          ]

  return (modDoc, Set.map crateNameToCargoText imports)

printInstances :: MonadRustBackend m => m [Doc ann]
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

printDerive :: MonadRustBackend m => PC.TyDefs -> PC.Derive -> m [Doc ann]
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
            printRsQTraitImpl mn iTyDefs hsqcn d
        )

printRsQTraitImpl :: MonadRustBackend m => PC.ModuleName -> PC.TyDefs -> R.QTraitName -> PC.Derive -> m (Doc ann)
printRsQTraitImpl mn iTyDefs hqcn d =
  case Map.lookup hqcn Rust.rsTraitImplPrinters of
    Nothing -> throwInternalError (d ^. #constraint . #sourceInfo) ("Missing capability to print the Rust trait " <> show hqcn) -- TODO(bladyjoker): Fix hqcn printing
    Just implPrinter -> do
      let ty = d ^. #constraint . #argument
      mkInstanceDoc <- printInstanceDef hqcn ty
      implPrinter mn iTyDefs mkInstanceDoc ty

printCompilationCfgs :: Pretty a => [a] -> Doc ann
printCompilationCfgs [] = mempty
printCompilationCfgs cfgs = vsep $ printCfg <$> cfgs
  where
    printCfg :: Pretty a => a -> Doc ann
    printCfg cfg = "#!" <> brackets (pretty cfg)

printImports :: Set R.CrateName -> Doc ann
printImports crates =
  vsep $ externCrate <$> Set.toList crates
  where
    externCrate :: R.CrateName -> Doc ann
    externCrate crateName = "extern crate" <+> pretty (crateNameToText crateName) <> semi

-- | `collectPackageDeps` collects all the package dependencies.
collectPackageDeps :: MonadRustBackend m => m (Set R.CrateName)
collectPackageDeps = do
  ctx <- ask
  st <- get
  let packages = rust'packages . view Print.ctxBackend $ ctx
      lbTyImports = ctx ^. Print.ctxTyImports
      rsTyImports = ctx ^. Print.ctxOpaqueTyImports <> st ^. Print.stTypeImports
      traitImps = ctx ^. Print.ctxClassImports <> st ^. Print.stClassImports
      ruleImps = ctx ^. Print.ctxRuleImports
      valImps = st ^. Print.stValueImports

  return $
    Set.filter
      (/= R.MkCrateName "crate")
      ( Set.singleton (R.MkCrateName "std")
          `Set.union` Set.fromList [R.crateFromLbModuleName packages $ withInfo mn | (mn, _tn) <- toList lbTyImports]
          `Set.union` Set.fromList (mapMaybe R.qualifiedToCrate $ toList rsTyImports)
          `Set.union` Set.fromList (mapMaybe R.qualifiedToCrate $ toList traitImps)
          `Set.union` Set.fromList [R.crateFromLbModuleName packages $ withInfo mn | mn <- toList ruleImps]
          `Set.union` Set.fromList (mapMaybe R.qualifiedToCrate $ toList valImps)
      )

withInfo :: PC.InfoLessC b => PC.InfoLess b -> b
withInfo x = PC.withInfoLess x id
