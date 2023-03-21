module LambdaBuffers.Frontend.Scope (collectImportedScope, collectLocalScope, addToClassScope, addToTyScope) where

import Control.Monad (foldM)
import Control.Monad.Trans.Reader (asks)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Frontend.Errors qualified as Errors
import LambdaBuffers.Frontend.Monad (FrontRead (current), FrontendT, throwE')
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Syntax (ClassDef (ClassDef), ClassName (ClassName), Import (Import, importModuleName), Module (moduleName, moduleStatements), ModuleAlias (ModuleAlias), ModuleName, Name (Name), SourceInfo, Statement (StClassDef, StTyDef), TyDef (TyDef), TyName (TyName))
import LambdaBuffers.Frontend.Utils (ClassScope, TyScope, strip)

addToTyScope :: TyScope -> (Import SourceInfo, TyScope) -> FrontendT m TyScope
addToTyScope scope (imp, addScope) =
  foldM
    ( \totalTyScope' tySym -> case Map.lookup tySym totalTyScope' of
        Nothing -> return $ Map.insert tySym (strip $ importModuleName imp) totalTyScope'
        Just alreadyInMn -> do
          cm <- asks current
          throwE' $ Errors.tyNameAlreadyImportedErr cm imp (snd tySym) alreadyInMn
    )
    scope
    (Map.keys addScope)

addToClassScope :: ClassScope -> (Import SourceInfo, ClassScope) -> FrontendT m ClassScope
addToClassScope scope (imp, addScope) =
  foldM
    ( \totalClassScope' classSym -> case Map.lookup classSym totalClassScope' of
        Nothing -> return $ Map.insert classSym (strip $ importModuleName imp) totalClassScope'
        Just alreadyInMn -> do
          cm <- asks current
          throwE' $ Errors.classNameAlreadyImportedErr cm imp (snd classSym) alreadyInMn
    )
    scope
    (Map.keys addScope)

type IdentifiedNames = (Set (TyName ()), Set (ClassName ()))

collectNamesInModule :: Module SourceInfo -> IdentifiedNames
collectNamesInModule m =
  ( Set.fromList [strip tyN | (StTyDef (TyDef tyN _ _ _)) <- moduleStatements m]
  , Set.fromList [strip clN | (StClassDef (ClassDef clN _ _ _)) <- moduleStatements m]
  )

identifyNames :: ModuleName SourceInfo -> [Name SourceInfo] -> IdentifiedNames -> FrontendT m IdentifiedNames
identifyNames impMn names identified = do
  cm <- asks current
  foldM
    ( \(tyNs, clNs) nm@(Name n _) -> do
        let tyN = TyName n ()
            clN = ClassName n ()
        case bimap (Set.member tyN) (Set.member clN) identified of
          (True, False) -> return (Set.insert tyN tyNs, clNs)
          (False, True) -> return (tyNs, Set.insert clN clNs)
          (True, True) -> return (Set.insert tyN tyNs, Set.insert clN clNs)
          (False, False) -> throwE' $ Errors.importedNotFoundErr cm impMn nm identified
    )
    (Set.empty, Set.empty)
    names

{- | `collectImportedScope impStmt impMod` builds a scope of symbols by interpreting the `impStmt` and looking into the imported module `impMod`
 1. Get all names in `impMod`,
 2. Check with `impStmt` if all the requested names are available and identify them,
 3. Build the type symbols and class symbols.
-}
collectImportedScope :: Import SourceInfo -> Module SourceInfo -> FrontendT m (TyScope, ClassScope)
collectImportedScope (Import isQual impMn mayImports mayAlias _) m = do
  let namesInModule = collectNamesInModule m
  (importedTyNames, importedClassNames) <- case mayImports of
    Nothing -> return namesInModule
    Just names -> identifyNames impMn names namesInModule
  importedTySyms <-
    foldM
      ( \total tyN ->
          return . Set.union total . Set.fromList $
            case mayAlias of
              Nothing ->
                if isQual
                  then
                    [ (Just $ ModuleAlias (strip impMn) (), tyN)
                    ]
                  else
                    [ (Just $ ModuleAlias (strip impMn) (), tyN)
                    , (Nothing, tyN)
                    ]
              Just al ->
                if isQual
                  then
                    [ (Just . strip $ al, tyN)
                    ]
                  else
                    [ (Just . strip $ al, tyN)
                    , (Nothing, tyN)
                    ]
      )
      mempty
      importedTyNames

  importedClassSyms <-
    foldM
      ( \total clN ->
          return . Set.union total . Set.fromList $
            case mayAlias of
              Nothing ->
                if isQual
                  then
                    [ (Just $ ModuleAlias (strip impMn) (), clN)
                    ]
                  else
                    [ (Just $ ModuleAlias (strip impMn) (), clN)
                    , (Nothing, clN)
                    ]
              Just al ->
                if isQual
                  then
                    [ (Just . strip $ al, clN)
                    ]
                  else
                    [ (Just . strip $ al, clN)
                    , (Nothing, clN)
                    ]
      )
      mempty
      importedClassNames

  return
    ( Map.fromList [(tySym, strip impMn) | tySym <- toList importedTySyms]
    , Map.fromList [(classSym, strip impMn) | classSym <- toList importedClassSyms]
    )

-- | Collects only the local scope, checking that there's no naming conflicts and dups.
collectLocalScope :: Module SourceInfo -> (TyScope, ClassScope) -> FrontendT m (TyScope, ClassScope)
collectLocalScope m (importedTySyms, importedClassSyms) = do
  localTySyms <-
    foldM
      ( \localTySyms tyDef@(TyDef tn _ _ _) -> do
          if Set.member (Nothing, strip tn) localTySyms
            then do
              cm <- asks current
              throwE' $ Errors.duplicateTyDefErr cm tyDef
            else case Map.lookup (Nothing, strip tn) importedTySyms of
              Nothing -> return $ Set.insert (Nothing, strip tn) localTySyms
              Just impMn -> do
                cm <- asks current
                throwE' $ Errors.tyDefNameConflictErr cm tyDef impMn
      )
      mempty
      [td | StTyDef td <- moduleStatements m]

  localClassSyms <-
    foldM
      ( \localClassSyms classDef@(ClassDef tn _ _ _) -> do
          if Set.member (Nothing, strip tn) localClassSyms
            then do
              cm <- asks current
              throwE' $ Errors.duplicateClassDefErr cm classDef
            else case Map.lookup (Nothing, strip tn) importedClassSyms of
              Nothing -> return $ Set.insert (Nothing, strip tn) localClassSyms
              Just impMn -> do
                cm <- asks current
                throwE' $ Errors.classDefNameConflictErr cm classDef impMn
      )
      mempty
      [cd | StClassDef cd <- moduleStatements m]

  return
    ( Map.fromList [(tySym, strip (moduleName m)) | tySym <- toList localTySyms]
    , Map.fromList [(classSym, strip (moduleName m)) | classSym <- toList localClassSyms]
    )
