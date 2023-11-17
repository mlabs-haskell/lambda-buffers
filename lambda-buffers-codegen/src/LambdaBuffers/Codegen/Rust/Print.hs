{- | `Print.MonadPrint` implementation for the Rust backend.

 The monad is instantiated with `R.QTyName` which are qualified Rust type
 names referring to `Opaque` type imports. It's also instantiated with
 `[R.QTraitName]` which denotes the qualified Rust class names to import.
 Note that a single LambdaBuffers 'class' can be unpacked into several related
 Rust classes and that's why it's a list of qualified Rust class names.
-}
module LambdaBuffers.Codegen.Rust.Print (MonadPrint, printModule, PrintModuleEnv (..)) where

import Control.Lens (view, (^.))
import Control.Monad.Reader.Class (ask, asks)
import Control.Monad.State.Class (MonadState (get))
import Data.Foldable (Foldable (toList), foldrM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (for)
import LambdaBuffers.Codegen.Config qualified as C
import LambdaBuffers.Codegen.Print (throwInternalError)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Rust.Print.InstanceDef (printInstanceDef)
import LambdaBuffers.Codegen.Rust.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Rust.Print.Syntax (crateNameToText)
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, brackets, comma, encloseSep, line, vsep)

data PrintModuleEnv m ann = PrintModuleEnv
  { env'printModuleName :: PC.ModuleName -> Doc ann
  , env'implementationPrinter ::
      Map
        R.QTraitName
        ( PC.ModuleName ->
          PC.TyDefs ->
          (Doc ann -> Doc ann) ->
          PC.Ty ->
          m (Doc ann)
        )
  , env'printTyDef :: MonadPrint m => PC.TyDef -> m (Doc ann)
  , env'compilationCfgs :: [Text]
  }

printModule :: MonadPrint m => PrintModuleEnv m ann -> m (Doc ann, Set Text)
printModule env = do
  ctx <- ask
  tyDefDocs <- for (toList $ ctx ^. Print.ctxModule . #typeDefs) (env'printTyDef env)
  instDocs <- printInstances env
  st <- get
  let modDoc =
        align . vsep $
          [ printCompilationCfgs (env'compilationCfgs env)
          , mempty
          , vsep ((line <>) <$> tyDefDocs)
          , mempty
          , vsep ((line <>) <$> instDocs)
          ]
      pkgDeps =
        collectPackageDeps
          (ctx ^. Print.ctxTyImports)
          (ctx ^. Print.ctxOpaqueTyImports <> st ^. Print.stTypeImports)
          (ctx ^. Print.ctxClassImports <> st ^. Print.stClassImports)
          (ctx ^. Print.ctxRuleImports)
          (st ^. Print.stValueImports)
  return (modDoc, pkgDeps)

printInstances :: MonadPrint m => PrintModuleEnv m ann -> m [Doc ann]
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

printDerive :: MonadPrint m => PrintModuleEnv m ann -> PC.TyDefs -> PC.Derive -> m [Doc ann]
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
            printRsQTraitImpl env mn iTyDefs hsqcn d
        )

printRsQTraitImpl :: MonadPrint m => PrintModuleEnv m ann -> PC.ModuleName -> PC.TyDefs -> R.QTraitName -> PC.Derive -> m (Doc ann)
printRsQTraitImpl env mn iTyDefs hqcn d =
  case Map.lookup hqcn (env'implementationPrinter env) of
    Nothing -> throwInternalError (d ^. #constraint . #sourceInfo) ("Missing capability to print the Rust trait " <> show hqcn) -- TODO(bladyjoker): Fix hqcn printing
    Just implPrinter -> do
      let ty = d ^. #constraint . #argument
          mkInstanceDoc = printInstanceDef hqcn ty
      implPrinter mn iTyDefs mkInstanceDoc ty

printCompilationCfgs :: Pretty a => [a] -> Doc ann
printCompilationCfgs [] = mempty
printCompilationCfgs exts = "#" <> brackets (align (encloseSep mempty mempty comma (pretty <$> exts)))

{- | `collectPackageDeps lbTyImports hsTyImports classImps ruleImps valImps` collects all the package dependencies.
 Note that LB `lbTyImports` and `ruleImps` are wired by the user (as the user decides on the package name for their schemass).
-}
collectPackageDeps :: Set PC.QTyName -> Set R.QTyName -> Set R.QTraitName -> Set (PC.InfoLess PC.ModuleName) -> Set R.QValName -> Set Text
collectPackageDeps _lbTyImports hsTyImports classImps _ruleImps valImps =
  let deps =
        Set.singleton "base"
          `Set.union` Set.fromList [crateNameToText cbl | (cbl, _, _) <- toList hsTyImports]
          `Set.union` Set.fromList [crateNameToText cbl | (cbl, _, _) <- toList classImps]
          `Set.union` Set.fromList [crateNameToText cbl | (cbl, _, _) <- toList valImps]
   in deps
