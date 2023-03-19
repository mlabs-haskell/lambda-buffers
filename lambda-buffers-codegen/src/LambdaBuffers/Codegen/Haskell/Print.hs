{- | `Print.MonadPrint` implementation for the Haskell backend.

 The monad is instantiated with `H.QTyName` which are qualified Haskell type
 names referring to `Opaque` type imports. It's also instantiated with
 `[H.QClassName]` which denotes the qualified Haskell class names to import.
 Note that a single LambdaBuffers 'class' can be unpacked into several related
 Haskell classes and that's why it's a list of qualified Haskell class names.
-}
module LambdaBuffers.Codegen.Haskell.Print (MonadPrint, printModule) where

import Control.Lens (view, (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Reader.Class (ask, asks)
import Data.Default (Default (def))
import Data.Foldable (Foldable (toList), foldrM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Codegen.Haskell.Print.Derive (printDeriveEq)
import LambdaBuffers.Codegen.Haskell.Print.InstanceDef (printInstanceDefs)
import LambdaBuffers.Codegen.Haskell.Print.Monad (MonadPrint)
import LambdaBuffers.Codegen.Haskell.Print.Names (printModName, printModName', printTyName)
import LambdaBuffers.Codegen.Haskell.Print.TyDef (printTyDef)
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Codegen.Print qualified as Print
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

classImplementations ::
  Map
    PC.QClassName
    ( PC.ModuleName ->
      PC.TyDefs ->
      Map H.QClassName (Doc ann -> Doc ann) ->
      PC.Ty ->
      Either Text (Doc ann, Set H.QValName)
    )
classImplementations =
  Map.fromList
    [
      ( (PC.mkInfoLess $ PC.ModuleName [PC.ModuleNamePart "Prelude" def] def, PC.mkInfoLess $ PC.ClassName "Eq" def)
      , printDeriveEq
      )
    ]

printInstances :: MonadPrint m => m ([Doc ann], Set H.QValName)
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

-- TODO(bladyjoker): This is too complicated.
printDerive :: MonadPrint m => PC.TyDefs -> PC.Derive -> m (Doc ann, Set H.QValName)
printDerive iTyDefs d = do
  mn <- asks (view $ Print.ctxModule . #moduleName)
  let qcn = PC.qualifyClassRef mn (d ^. #constraint . #classRef)
  case Map.lookup qcn classImplementations of
    Nothing -> throwError (d ^. #constraint . #sourceInfo, "TODO(bladyjoker): Missing capability to print " <> (Text.pack . show $ qcn))
    Just implPrinter -> do
      mkInstanceDocs <- printInstanceDefs d
      case implPrinter mn iTyDefs mkInstanceDocs (d ^. #constraint . #argument) of
        Left err -> throwError (d ^. #constraint . #sourceInfo, "Failed printing the implementation\n" <> err)
        Right (instanceDefsDoc, valImps) -> return (instanceDefsDoc, valImps)

printModuleHeader :: PC.ModuleName -> Set (PC.InfoLess PC.TyName) -> Doc ann
printModuleHeader mn exports =
  let typeExportsDoc = align $ group $ encloseSep lparen rparen (comma <> space) ((`PC.withInfoLess` printTyName) <$> toList exports)
   in "module" <+> printModName mn <+> typeExportsDoc <+> "where"

-- TODO(bladyjoker): Collect package dependencies.
printImports :: Set PC.QTyName -> Set H.QTyName -> Set [H.QClassName] -> Set (PC.InfoLess PC.ModuleName) -> Set H.QValName -> Doc ann
printImports lbTyImports hsTyImports classImps ruleImps valImps =
  let groupedLbImports =
        Set.fromList [mn | (mn, _tn) <- toList lbTyImports]
          `Set.union` ruleImps
      lbImportDocs = importQualified . printModName' <$> toList groupedLbImports

      groupedHsImports =
        Set.fromList [mn | (_cbl, mn, _tn) <- toList hsTyImports]
          `Set.union` Set.fromList [mn | cImps <- toList classImps, (_, mn, _) <- cImps]
          `Set.union` Set.fromList [mn | (_, mn, _) <- toList valImps]
      hsImportDocs = (\(H.MkModuleName mn) -> importQualified $ pretty mn) <$> toList groupedHsImports

      importsDoc = vsep $ lbImportDocs ++ hsImportDocs
   in importsDoc
  where
    importQualified :: Doc ann -> Doc ann
    importQualified mnDoc = "import qualified" <+> mnDoc
