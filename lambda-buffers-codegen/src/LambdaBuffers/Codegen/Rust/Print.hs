{- | `Print.MonadPrint` implementation for the Rust backend.

 The monad is instantiated with `R.QTyName` which are qualified Rust type
 names referring to `Opaque` type imports. It's also instantiated with
 `[R.QTraitName]` which denotes the qualified Rust trait names to import.
 Note that a single LambdaBuffers 'trait' can be unpacked into several related
 Rust traites and that's why it's a list of qualified Rust trait names.
-}
module LambdaBuffers.Codegen.Rust.Print (MonadPrint, printModule, PrintModuleEnv (..)) where

import Control.Lens (view, (^.))
import Control.Monad.Reader.Class (ask, asks)
import Control.Monad.State.Class (MonadState (get))
import Data.Foldable (Foldable (toList), foldrM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (for)
import LambdaBuffers.Codegen.Config qualified as C
import LambdaBuffers.Codegen.Print (throwInternalError)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Rust.Print.InstanceDef (printInstanceDef)
import LambdaBuffers.Codegen.Rust.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Rust.Print.Syntax (crateNameToCargoText, crateNameToText)
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, brackets, line, semi, vsep, (<+>))

data PrintModuleEnv m ann = PrintModuleEnv
  { env'printModuleName :: PC.ModuleName -> Doc ann
  , env'implementationPrinter ::
      Map
        R.QTraitName
        ( PC.ModuleName ->
          R.PkgMap ->
          PC.TyDefs ->
          (Doc ann -> Doc ann) ->
          PC.Ty ->
          m (Doc ann)
        )
  , env'printTyDef :: MonadPrint m => R.PkgMap -> PC.TyDef -> m (Doc ann)
  , env'compilationCfgs :: [Text]
  }

printModule :: MonadPrint m => PrintModuleEnv m ann -> R.PkgMap -> m (Doc ann, Set Text)
printModule env pkgs = do
  ctx <- ask
  tyDefDocs <- for (toList $ ctx ^. Print.ctxModule . #typeDefs) (env'printTyDef env pkgs)
  instDocs <- printInstances env pkgs
  st <- get
  let modDoc =
        align . vsep $
          [ printCompilationCfgs (env'compilationCfgs env)
          , printImports imports
          , mempty
          , vsep ((line <>) <$> tyDefDocs)
          , mempty
          , vsep ((line <>) <$> instDocs)
          ]
      imports =
        collectPackageDeps
          pkgs
          (ctx ^. Print.ctxTyImports)
          (ctx ^. Print.ctxOpaqueTyImports <> st ^. Print.stTypeImports)
          (ctx ^. Print.ctxClassImports <> st ^. Print.stClassImports)
          (ctx ^. Print.ctxRuleImports)
          (st ^. Print.stValueImports)

  return (modDoc, Set.map crateNameToCargoText imports)

printInstances :: MonadPrint m => PrintModuleEnv m ann -> R.PkgMap -> m [Doc ann]
printInstances env pkgs = do
  ci <- asks (view Print.ctxCompilerInput)
  m <- asks (view Print.ctxModule)
  let iTyDefs = PC.indexTyDefs ci
  foldrM
    ( \d instDocs -> do
        instDocs' <- printDerive env pkgs iTyDefs d
        return $ instDocs' <> instDocs
    )
    mempty
    (toList $ m ^. #derives)

printDerive :: MonadPrint m => PrintModuleEnv m ann -> R.PkgMap -> PC.TyDefs -> PC.Derive -> m [Doc ann]
printDerive env pkgs iTyDefs d = do
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
            printRsQTraitImpl env mn pkgs iTyDefs hsqcn d
        )

printRsQTraitImpl :: MonadPrint m => PrintModuleEnv m ann -> PC.ModuleName -> R.PkgMap -> PC.TyDefs -> R.QTraitName -> PC.Derive -> m (Doc ann)
printRsQTraitImpl env mn pkgs iTyDefs hqcn d =
  case Map.lookup hqcn (env'implementationPrinter env) of
    Nothing -> throwInternalError (d ^. #constraint . #sourceInfo) ("Missing capability to print the Rust trait " <> show hqcn) -- TODO(bladyjoker): Fix hqcn printing
    Just implPrinter -> do
      let ty = d ^. #constraint . #argument
          mkInstanceDoc = printInstanceDef pkgs hqcn ty
      implPrinter mn pkgs iTyDefs mkInstanceDoc ty

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

-- | `collectPackageDeps lbTyImports rsTyImports traitImps ruleImps valImps` collects all the package dependencies.
collectPackageDeps :: R.PkgMap -> Set PC.QTyName -> Set R.QTyName -> Set R.QTraitName -> Set (PC.InfoLess PC.ModuleName) -> Set R.QValName -> Set R.CrateName
collectPackageDeps packages lbTyImports rsTyImports traitImps ruleImps valImps =
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
