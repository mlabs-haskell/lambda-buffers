module LambdaBuffers.Codegen.Typescript.Print (printModule) where

import Control.Arrow ((***))
import Control.Lens (view, (^.), (^..))
import Control.Lens qualified as Lens
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
import LambdaBuffers.Codegen.Print (throwInternalError)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Typescript.Backend (MonadTypescriptBackend)
import LambdaBuffers.Codegen.Typescript.Print.Derive (printDeriveEq, printDeriveIsPlutusData, printDeriveJson, tsEqClass, tsIsPlutusDataClass, tsJsonClass)
import LambdaBuffers.Codegen.Typescript.Print.InstanceDef (printExportInstanceDecl)
import LambdaBuffers.Codegen.Typescript.Print.Names (printModName, printModName')
import LambdaBuffers.Codegen.Typescript.Print.TyDef (printTyDef)
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), squotes, vsep, (<+>))
import Proto.Codegen qualified as P

printModule :: MonadTypescriptBackend m => Ts.PkgMap -> m (Doc ann, Set Text)
printModule pkgMap = do
  ctx <- ask
  tyDefDocs <- printTyDefs pkgMap (ctx ^. Print.ctxModule)
  instDocs <- printInstances pkgMap
  st <- get

  let modDoc =
        vsep $
          -- TODO(jaredponn): the `@ts-nocheck` disables all semantic checking from TypeScript
          -- https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-7.html#-ts-nocheck-in-typescript-files
          -- In the future, how about we properly resolve this to convince
          -- TypeScript that the generated code is okay....
          --
          -- Known problems include:
          --    - Generated code like:
          --      @
          --        (x) => { if (x.name == 'case1') { ... } else if (x.name == 'case2') { .. }  }
          --      @
          --      fails exhaustiveness checks since Typescript "generalizes"
          --      @x@ to be _any_ type instead of the specific sumtype it is
          --      instantiated with.
          --
          --      But really, it is fine since we will only call the lambda
          --      with the right type.

          [ "// @ts-nocheck"
          , printSelfImport (ctx ^. Print.ctxModule . #moduleName)
          , printImports
              (ctx ^. Print.ctxModule . #moduleName)
              pkgMap
              (ctx ^. Print.ctxTyImports)
              (ctx ^. Print.ctxOpaqueTyImports)
              (ctx ^. Print.ctxClassImports <> st ^. Print.stClassImports)
              (ctx ^. Print.ctxRuleImports)
              (st ^. Print.stValueImports)
          , mempty
          ]
            ++ tyDefDocs
            ++ instDocs
      pkgDeps =
        collectPackageDeps
          pkgMap
          (ctx ^. Print.ctxTyImports)
          (ctx ^. Print.ctxOpaqueTyImports)
          (ctx ^. Print.ctxClassImports <> st ^. Print.stClassImports)
          (ctx ^. Print.ctxRuleImports)
          (st ^. Print.stValueImports)
  return (modDoc, pkgDeps)

printTyDefs :: MonadTypescriptBackend m => Ts.PkgMap -> PC.Module -> m [Doc ann]
printTyDefs pkgMap m = for (toList $ m ^. #typeDefs) $ printTyDef pkgMap

tsClassImplPrinters ::
  Ts.PkgMap ->
  Map
    Ts.QClassName
    ( PC.ModuleName ->
      PC.TyDefs ->
      (Doc ann -> Doc ann) ->
      PC.Ty ->
      Either P.InternalError (Doc ann, Set Ts.QValName)
    )
tsClassImplPrinters pkgMap =
  Map.fromList
    [
      ( tsEqClass
      , printDeriveEq pkgMap
      )
    ,
      ( tsIsPlutusDataClass
      , printDeriveIsPlutusData pkgMap
      )
    ,
      ( tsJsonClass
      , printDeriveJson pkgMap
      )
    ]

printInstances :: MonadTypescriptBackend m => Ts.PkgMap -> m [Doc ann]
printInstances pkgMap = do
  ci <- asks (view Print.ctxCompilerInput)
  m <- asks (view Print.ctxModule)
  let iTyDefs = PC.indexTyDefs ci
  foldrM
    ( \d instDocs -> do
        instDocs' <- printDerive pkgMap iTyDefs d
        return $ instDocs' <> instDocs
    )
    mempty
    (toList $ m ^. #derives)

printDerive :: MonadTypescriptBackend m => Ts.PkgMap -> PC.TyDefs -> PC.Derive -> m [Doc ann]
printDerive pkgMap iTyDefs d = do
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
            printTsQClassImpl pkgMap mn iTyDefs pqcn d
        )

printTsQClassImpl :: MonadTypescriptBackend m => Ts.PkgMap -> PC.ModuleName -> PC.TyDefs -> Ts.QClassName -> PC.Derive -> m (Doc ann)
printTsQClassImpl pkgMap mn iTyDefs hqcn d =
  case Map.lookup hqcn (tsClassImplPrinters pkgMap) of
    Nothing -> throwInternalError (d ^. #constraint . #sourceInfo) ("Missing capability to print the Typescript type class " <> show hqcn)
    Just implPrinter -> do
      let ty = d ^. #constraint . #argument
      mkInstanceDoc <- printExportInstanceDecl pkgMap hqcn ty
      case implPrinter mn iTyDefs mkInstanceDoc ty of
        Left err -> throwInternalError (d ^. #constraint . #sourceInfo) ("Failed printing the implementation for " <> (show hqcn <> "\nGot error: " <> show err))
        Right (instanceDefsDoc, valImps) -> do
          for_ (toList valImps) Print.importValue
          return instanceDefsDoc

lbTsExt :: Doc ann
lbTsExt = ".mjs"

printSelfImport :: PC.ModuleName -> Doc ann
printSelfImport modName = case PC.parts modName of
  [] -> "TODO(jaredponn): Invalid empty module name"
  allParts ->
    "import"
      <+> "*"
      <+> "as"
      <+> printModName modName
      <+> "from"
      <+> squotes ("./" <> pretty (last allParts ^. #name) <> lbTsExt)

printImports ::
  PC.ModuleName ->
  Ts.PkgMap ->
  Set PC.QTyName ->
  Set Ts.QTyName ->
  Set Ts.QClassName ->
  Set (PC.InfoLess PC.ModuleName) ->
  Set Ts.QValName ->
  Doc ann
printImports selfModName pkgMap lbTyImports tsTyImports classImps ruleImps valImps =
  let groupedLbImports =
        Set.fromList [mn | (mn, _tn) <- toList lbTyImports]
          `Set.union` ruleImps
      lbImportDocs =
        importQualified
          . ( \mn ->
                ( case Map.lookup mn pkgMap of
                    Nothing ->
                      PC.withInfoLess
                        mn
                        ( \mn' ->
                            -- Given @A/B/C/D@ and @A/B/E/F/G@, this would get
                            -- @(D, E/F/G)@
                            let getUncommonSuffix :: Eq a => [a] -> [a] -> ([a], [a])
                                getUncommonSuffix (l : ls) (r : rs)
                                  | l == r = getUncommonSuffix ls rs
                                  | otherwise = (l : ls, r : rs)
                                getUncommonSuffix l r = (l, r)

                                (selfUncommonSuffix, mnUncommonSuffix) = getUncommonSuffix (selfModName ^. #parts) (mn' ^. #parts)
                             in "./"
                                  <>
                                  -- Move back to the first uncommon
                                  -- directory, then put the uncommon suffix.
                                  pretty
                                    ( Text.intercalate "/" $
                                        drop 1 (map (const "..") selfUncommonSuffix)
                                          ++ map (view #name) mnUncommonSuffix
                                    )
                                  <> lbTsExt
                        )
                    Just pkgName ->
                      -- Print something like
                      -- @
                      -- pkg-name/LambdaBuffers/path/to/module/name.mjs
                      -- @
                      -- Why do we include @LambdaBuffers@ there? See
                      -- "LambdaBuffers.Codegen.Typescript.Syntax.filepathFromModuleName"
                      pretty (Ts.pkgNameToText pkgName)
                        <> "/"
                        <> "LambdaBuffers"
                        <> "/"
                        <> PC.withInfoLess mn (\mn' -> pretty $ Text.intercalate "/" (mn' ^.. #parts . Lens.traversed . #name))
                        <> lbTsExt
                , printModName' mn
                )
            )
          <$> toList groupedLbImports

      groupedTsImports =
        -- TODO(jaredponn): resolve this awkward maybe situation for the
        -- PackageName. We really should never have the Nothing case...
        Set.fromList [(pkg, mn) | (Just pkg, mn, _tn) <- toList tsTyImports]
          `Set.union` Set.fromList [(pkg, mn) | (pkg, mn, _) <- toList classImps]
          `Set.union` Set.fromList [(pkg, mn) | (Just (pkg, mn), _) <- toList valImps]

      tsImportDocs = importQualified . ((\(Ts.MkPackageName pkg) -> pretty pkg) *** (\(Ts.MkModuleName mn) -> pretty mn)) <$> toList groupedTsImports

      importsDoc = vsep $ lbImportDocs ++ tsImportDocs
   in importsDoc
  where
    importQualified :: (Doc ann, Doc ann) -> Doc ann
    importQualified (pkg, mn) = "import" <+> "*" <+> "as" <+> mn <+> "from" <+> squotes pkg

{- | `collectPackageDeps pkgMap lbTyImports tsTyImports classImps ruleImps valImps` collects all the package dependencies.
 Note that LB `lbTyImports` and `ruleImps` are wired by the user (as the user decides on the package name for their schemas).
-}
collectPackageDeps :: Ts.PkgMap -> Set PC.QTyName -> Set Ts.QTyName -> Set Ts.QClassName -> Set (PC.InfoLess PC.ModuleName) -> Set Ts.QValName -> Set Text
collectPackageDeps pkgMap lbTyImports tsTyImports classImps ruleImps valImps =
  let deps =
        Set.fromList [Ts.pkgNameToText pkgName | (Just pkgName, _, _) <- toList tsTyImports]
          `Set.union` Set.fromList [Ts.pkgNameToText pkgName | (pkgName, _, _) <- toList classImps]
          `Set.union` Set.fromList [Ts.pkgNameToText pkgName | (Just (pkgName, _), _) <- toList valImps]
          `Set.union` Set.fromList
            ( toList (Set.map fst lbTyImports `Set.union` ruleImps) >>= \moduleName ->
                case Map.lookup moduleName pkgMap of
                  Just pkgName -> return $ Ts.pkgNameToText pkgName
                  -- If there is no module, then we assume that the qualified
                  -- identifier is a builtin and hence requires no dependencies
                  Nothing -> []
            )
   in deps
