module LambdaBuffers.Codegen.Haskell.TyDef (
  printTyDef,
  printSum,
  printModule,
  runPrint,
) where

import Control.Lens (view, (&), (.~), (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExcept)
import Control.Monad.RWS (RWST (runRWST))
import Control.Monad.RWS.Class (MonadRWS, asks)
import Control.Monad.Reader.Class (MonadReader (local))
import Control.Monad.State.Class (modify)
import Control.Monad.Writer.Class (MonadWriter (tell))
import Data.Char qualified as Char
import Data.Foldable (Foldable (toList), for_)
import Data.Generics.Labels ()
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Codegen.Haskell.Config (Config (MkConfig), classes, opaques)
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Compiler.ProtoCompat (CompilerInput, runFromProto)
import LambdaBuffers.Compiler.ProtoCompat.Types (ClassDef, ClassName (ClassName), ConstrName (ConstrName), Constraint (Constraint), Constructor (Constructor), Field (Field), FieldName (FieldName), ForeignRef (ForeignRef), InstanceClause, LocalRef (LocalRef), Module, ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Product (RecordI, TupleI), Record (Record), Sum (Sum), Tuple (Tuple), Ty (TyAppI, TyRefI, TyVarI), TyAbs (TyAbs), TyApp (TyApp), TyArg (TyArg), TyBody (OpaqueI, SumI), TyClassRef (ForeignCI, LocalCI), TyDef, TyName (TyName), TyRef (ForeignI, LocalI), TyVar (TyVar), VarName (VarName))
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, concatWith, dot, encloseSep, equals, group, lbrace, line, lparen, parens, pipe, rbrace, rparen, sep, space, squote, surround, vsep, (<+>))
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

data PrintCtx
  = ModuleCtx
  | TyDefCtx TyDef
  | InstanceClauseCtx ModuleName
  deriving stock (Eq, Ord, Show)

type PrintRead = (Config, PrintCtx)

type PrintWrite = [PrintCommand]
data PrintCommand
  = AddTyDef (Doc ())
  | AddInstanceDef (Doc ())
  deriving stock (Show)

type PrintErr = String

data PrintState = MkPrintState
  { moduleTyImports :: Set (H.CabalPackageName, H.ModuleName, H.TyName)
  , moduleTyExports :: Set H.TyName
  , moduleClassImports :: Set (H.CabalPackageName, H.ModuleName, H.ClassName, [H.FunctionName])
  }
  deriving stock (Eq, Ord, Show)

type MonadPrint m = (MonadRWS PrintRead PrintWrite PrintState m, MonadError PrintErr m)

runPrint :: Config -> Module -> Either PrintErr (Doc ())
runPrint cfg m =
  let p = runRWST (printModule m) (cfg, ModuleCtx) (MkPrintState mempty mempty mempty)
      p' = runExcept p
   in printAll <$> p'
  where
    printHeader :: H.ModuleName -> PrintState -> Doc ()
    printHeader (H.MkModuleName mn) ps =
      let typeExportsDoc = align $ group $ encloseSep lparen rparen comma ((\(H.MkTyName tn) -> pretty tn) <$> toList (moduleTyExports ps))
       in "module" <+> pretty mn <+> typeExportsDoc <+> "where"

    printImports :: PrintState -> Doc ()
    printImports ps =
      let groupedByCabalAndModuleName = Map.unionsWith Set.union [Map.singleton (c, mn) (Set.singleton tn) | (c, mn, tn) <- toList (moduleTyImports ps)]
          typeImportsDocs = (\((_, H.MkModuleName mn), tns) -> "import qualified" <+> pretty mn <+> encloseSep lparen rparen comma ((\(H.MkTyName tn) -> pretty tn) <$> toList tns)) <$> Map.toList groupedByCabalAndModuleName
          typeImportsDoc = vsep typeImportsDocs
       in typeImportsDoc

    printTyDefs :: [PrintCommand] -> Doc ()
    printTyDefs cs = vsep [d | AddTyDef d <- cs]

    printInstanceDefs :: [PrintCommand] -> Doc ()
    printInstanceDefs cs = vsep [d | AddInstanceDef d <- cs]

    printAll :: (H.ModuleName, PrintState, PrintWrite) -> Doc ()
    printAll (mn, ps, pw) =
      vsep
        [ printHeader mn ps
        , line
        , printImports ps
        , line
        , printTyDefs pw
        , line
        , printInstanceDefs pw
        , line
        ]

askConfig :: MonadPrint m => m Config
askConfig = asks fst

askCtx :: MonadPrint m => m PrintCtx
askCtx = asks snd

askTyDefCtx :: MonadPrint m => m TyDef
askTyDefCtx = do
  ctx <- askCtx
  case ctx of
    TyDefCtx td -> return td
    other -> throwError $ "Internal error, wanted TyDefCtx got " <> show other

askInstCtx :: MonadPrint m => m ModuleName
askInstCtx = do
  ctx <- askCtx
  case ctx of
    InstanceClauseCtx mn -> return mn
    other -> throwError $ "Internal error, wanted InstanceClauseCtx got " <> show other

printModule :: MonadPrint m => Module -> m H.ModuleName
printModule m = do
  for_ (m ^. #typeDefs) (\td -> local (\(cfg, _) -> (cfg, TyDefCtx td)) (printTyDef td))
  for_ (m ^. #classDefs) $ local (\(cfg, _) -> (cfg, InstanceClauseCtx $ m ^. #moduleName)) . printClassDef
  for_ (m ^. #instances) $ local (\(cfg, _) -> (cfg, InstanceClauseCtx $ m ^. #moduleName)) . printInstanceClause
  return $ lbModuleNameToHaskModName (m ^. #moduleName)

printTyDef :: MonadPrint m => TyDef -> m ()
printTyDef td = printTyAbs $ td ^. #tyAbs

exportTy :: MonadPrint m => H.TyName -> m ()
exportTy htyN = modify (\s -> s {moduleTyExports = Set.union (moduleTyExports s) (Set.singleton htyN)})

importTy :: MonadPrint m => (H.CabalPackageName, H.ModuleName, H.TyName) -> m ()
importTy qhTyRef = modify (\s -> s {moduleTyImports = Set.union (moduleTyImports s) (Set.singleton qhTyRef)})

importClass :: MonadPrint m => (H.CabalPackageName, H.ModuleName, H.ClassName, [H.FunctionName]) -> m ()
importClass qhClassRef = modify (\s -> s {moduleClassImports = Set.union (moduleClassImports s) (Set.singleton qhClassRef)})

printTyAbs :: MonadPrint m => TyAbs -> m ()
printTyAbs (TyAbs _ (OpaqueI _) _) = do
  cfg <- askConfig
  tn <- view #tyName <$> askTyDefCtx
  qhTyRef@(_, H.MkModuleName hsMn, H.MkTyName hsTn) <- case Map.lookup tn (cfg ^. opaques) of
    Nothing -> throwError $ "TODO(bladyjoker): Opaque not configured" <> show tn
    Just qhsRef -> return qhsRef
  exportTy (H.MkTyName $ tn ^. #name)
  importTy qhTyRef
  tell
    [ AddTyDef $ "type" <+> printTyName tn <+> equals <+> pretty hsMn <> dot <> pretty hsTn
    ]
printTyAbs (TyAbs args (SumI s) _) = do
  sumDoc <- printSum s
  td <- askTyDefCtx
  let argsDoc = sep (printTyArg <$> toList args) -- FIXME(bladyjoker): OMap on Constructors
      tdDoc = group $ "data" <+> printTyName (td ^. #tyName) <+> argsDoc <+> equals <+> sumDoc
  exportTy (H.MkTyName $ td ^. #tyName . #name)
  tell
    [ AddTyDef tdDoc
    ]
  where
    printTyArg :: forall {a}. TyArg -> Doc a
    printTyArg (TyArg vn _ _) = printVarName vn

printSum :: MonadPrint m => Sum -> m (Doc ())
printSum (Sum ctors _) = do
  ctorDocs <- for (toList ctors) printCtor -- FIXME(bladyjoker): OMap on Constructors
  return $
    group $
      if null ctors
        then mempty
        else align $ encloseSep mempty mempty (space <> pipe <> space) ctorDocs

printCtor :: MonadPrint m => Constructor -> m (Doc a)
printCtor (Constructor ctorName prod) = do
  ctorNDoc <- printCtorName ctorName
  prodDoc <- printProd prod
  return $ align $ group (ctorNDoc <+> prodDoc)

{- | Translate LambdaBuffer sum constructor names into Haskell sum constructor names
 sum Sum = Foo Int | Bar String
 translates to
 data Sum = Sum'Foo Int | Sum'Bar String
-}
printCtorName :: MonadPrint m => ConstrName -> m (Doc a)
printCtorName (ConstrName n _) = do
  tn <- view #tyName <$> askTyDefCtx
  return $ group $ printTyName tn <> squote <> pretty n

printProd :: MonadPrint m => Product -> m (Doc a)
printProd (RecordI rc) = printRec rc
printProd (TupleI tup) = printTup tup

printRec :: MonadPrint m => Record -> m (Doc a)
printRec (Record fields _) = do
  fieldDocs <- for (toList fields) printField -- FIXME(bladyjoker): OMap on Fields
  return $ group $ encloseSep lbrace rbrace (space <> comma <> space) fieldDocs

printTup :: MonadPrint m => Tuple -> m (Doc a)
printTup (Tuple fields _) = do
  tyDocs <- for fields printTy
  return $ group $ sep tyDocs

printField :: MonadPrint m => Field -> m (Doc a)
printField (Field fn ty) = do
  fieldNDoc <- printFieldName fn
  tyDoc <- printTy ty
  return $ fieldNDoc <+> colon <> colon <+> tyDoc

{- | Translate LambdaBuffer record field names into Haskell record field names
 rec Rec = { foo :: Int, bar :: String }
 translates to
 data Rec = MkRec { rec'foo :: Int, rec'bar :: String }
-}
printFieldName :: MonadPrint m => FieldName -> m (Doc a)
printFieldName (FieldName n _) = do
  tn <- view #tyName <$> askTyDefCtx
  _ <- case Text.uncons (tn ^. #name) of
    Nothing -> throwError $ "Internal error: received an empty TyName: " <> show tn
    Just (h, t) -> return $ Text.cons (Char.toLower h) t
  return $ pretty (tn ^. #name) <> squote <> pretty n

printTy :: MonadPrint m => Ty -> m (Doc a)
printTy (TyVarI v) = return $ printTyVar v
printTy (TyRefI r) = printTyRef r
printTy (TyAppI a) = printTyApp a

printTyApp :: MonadPrint m => TyApp -> m (Doc a)
printTyApp (TyApp f args _) = do
  fDoc <- printTy f
  argsDoc <- for args printTy
  return $ group $ parens $ fDoc <+> align (sep argsDoc)

printTyRef :: MonadPrint m => TyRef -> m (Doc a)
printTyRef (LocalI (LocalRef tn _)) = return $ group $ printTyName tn
printTyRef (ForeignI fr@(ForeignRef tn mn _)) = do
  importTy (foreignTyRefToHaskImport fr)
  return $ group $ printModName mn <> dot <> printTyName tn

foreignTyRefToHaskImport :: ForeignRef -> (H.CabalPackageName, H.ModuleName, H.TyName)
foreignTyRefToHaskImport fr =
  ( lbModuleNameToCabalPackageName $ fr ^. #moduleName
  , lbModuleNameToHaskModName $ fr ^. #moduleName
  , H.MkTyName $ fr ^. #tyName . #name
  )

lbModuleNameToHaskModName :: ModuleName -> H.ModuleName
lbModuleNameToHaskModName mn = H.MkModuleName $ Text.intercalate "." ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts])

lbModuleNameToCabalPackageName :: ModuleName -> H.CabalPackageName
lbModuleNameToCabalPackageName mn = H.MkCabalPackageName $ Text.intercalate "-" ([Text.toLower $ p ^. #name | p <- mn ^. #parts] <> ["-lb"])

printTyVar :: TyVar -> Doc a
printTyVar (TyVar vn _) = printVarName vn

printVarName :: VarName -> Doc a
printVarName (VarName n _) = pretty n

printTyName :: TyName -> Doc a
printTyName (TyName n _) = pretty n

printModName :: ModuleName -> Doc a
printModName (ModuleName parts _) = group $ concatWith (surround dot) [pretty p | ModuleNamePart p _ <- parts]

{- Typeclasses -}

{- | Print a ClassDef
 Checks whether the mapping is configured and doesn't print anything.
 TODO(bladyjoker): Do we need to import all the superclasses?
-}
printClassDef :: MonadPrint m => ClassDef -> m ()
printClassDef cd = do
  cfg <- askConfig
  localModName <- askInstCtx
  case Map.lookup (localModName, cd ^. #className) (cfg ^. classes) of
    Nothing -> throwError $ "TODO(bladyjoker): Type class not configured" <> show cd
    Just _ -> return ()

-- FIXME(bladyjoker): Reformulate InstanceClause into InstanceDef
-- message InstanceDef {
--   Constraint head = 1;
--   repeated Constraint body = 2;
-- }
printInstanceClause :: MonadPrint m => InstanceClause -> m ()
printInstanceClause ic = do
  headDoc <- printConstraint (Constraint (ic ^. #classRef) (ic ^. #head) (ic ^. #sourceInfo))
  cDocs <- for (ic ^. #constraints) printConstraint
  (_, _, _, fnNs) <- resolveClassRef $ ic ^. #classRef
  let bodyDoc = if null cDocs then mempty else encloseSep lparen rparen comma cDocs <> space
      implDoc =
        if null fnNs
          then mempty
          else space <> "where" <> line <> space <> space <> align (vsep (implDoc' <$> fnNs))
      implDoc' :: H.FunctionName -> Doc ann
      implDoc' (H.MkFunctionName fnN) = pretty fnN <+> equals <+> "error \"not implemented\""
  tell [AddInstanceDef $ "instance" <+> bodyDoc <> headDoc <> implDoc]

printConstraint :: MonadPrint m => Constraint -> m (Doc a)
printConstraint c = do
  crefDoc <- printClassRef (c ^. #classRef)
  headDoc <- printTy $ c ^. #argument
  return $ crefDoc <+> headDoc

printClassRef :: MonadPrint m => TyClassRef -> m (Doc a)
printClassRef cr = do
  (_, H.MkModuleName hmn, H.MkClassName hcn, _) <- resolveClassRef cr
  return $ pretty hmn <> dot <> pretty hcn

resolveClassRef :: MonadPrint m => TyClassRef -> m (H.CabalPackageName, H.ModuleName, H.ClassName, [H.FunctionName])
resolveClassRef cr = do
  cfg <- askConfig
  mc <- case cr of
    LocalCI lcr -> (,lcr ^. #className) <$> askInstCtx
    ForeignCI fcr -> return (fcr ^. #moduleName, fcr ^. #className)
  case Map.lookup mc (cfg ^. classes) of
    Nothing -> throwError $ "TODO(bladyjoker): Failed resolving a ty class reference " <> show cr
    Just qhClassRef -> do
      importClass qhClassRef
      return qhClassRef

testCompInp :: Either P.CompilerError CompilerInput
testCompInp =
  runFromProto $
    defMessage
      & P.modules
        .~ [ defMessage
              & P.moduleName . P.parts .~ [defMessage & P.name .~ "TestMod"]
              & P.typeDefs
                .~ [ defMessage
                      & P.tyName . P.name .~ "I8"
                      & P.tyAbs . P.tyBody . P.opaque .~ defMessage
                   , defMessage
                      & P.tyName . P.name .~ "Set"
                      & P.tyAbs . P.tyArgs .~ [mkArg "a"]
                      & P.tyAbs . P.tyBody . P.opaque .~ defMessage
                   , defMessage
                      & P.tyName . P.name .~ "Maybe"
                      & P.tyAbs . P.tyArgs .~ [mkArg "a"]
                      & P.tyAbs . P.tyBody . P.sum . P.constructors
                        .~ [ defMessage
                              & P.constrName . P.name .~ "Nothing"
                              & P.product . P.ntuple . P.fields .~ []
                           , defMessage
                              & P.constrName . P.name .~ "Just"
                              & P.product . P.ntuple . P.fields .~ [mkTyVar "a"]
                           ]
                   , defMessage
                      & P.tyName . P.name .~ "Either"
                      & P.tyAbs . P.tyArgs .~ [mkArg "a", mkArg "b"]
                      & P.tyAbs . P.tyBody . P.sum . P.constructors
                        .~ [ defMessage
                              & P.constrName . P.name .~ "Left"
                              & P.product . P.ntuple . P.fields .~ [mkTyVar "a"]
                           , defMessage
                              & P.constrName . P.name .~ "Right"
                              & P.product . P.ntuple . P.fields .~ [mkTyVar "b"]
                           ]
                   ]
              & P.classDefs
                .~ [ defMessage
                      & P.className . P.name .~ "Eq"
                      & P.classArgs .~ [mkArg "a"]
                   , defMessage
                      & P.className . P.name .~ "Ord"
                      & P.classArgs .~ [mkArg "a"]
                   ]
              & P.instances
                .~ [ defMessage
                      & P.classRef . P.localClassRef . P.className . P.name .~ "Eq"
                      & P.args
                        .~ [ defMessage
                              & P.tyApp . P.tyFunc . P.tyRef . P.localTyRef . P.tyName . P.name .~ "Maybe"
                              & P.tyApp . P.tyArgs .~ [mkTyVar "a"]
                           ]
                   ]
           ]
  where
    mkArg :: Text -> P.TyArg
    mkArg vn =
      defMessage
        & P.argName . P.name .~ vn
        & P.argKind . P.kindRef .~ P.Kind'KIND_REF_TYPE

    mkTyVar :: Text -> P.Ty
    mkTyVar vn = defMessage & P.tyVar . P.varName . P.name .~ vn

testConfig :: Config
testConfig =
  MkConfig
    ( Map.fromList
        [ (TyName "I8" PC.defSourceInfo, (H.MkCabalPackageName "base", H.MkModuleName "Data.Int", H.MkTyName "Int8"))
        , (TyName "Set" PC.defSourceInfo, (H.MkCabalPackageName "containers", H.MkModuleName "Data.Set", H.MkTyName "Set"))
        ]
    )
    ( Map.fromList
        [ ((ModuleName [ModuleNamePart "TestMod" PC.defSourceInfo] PC.defSourceInfo, ClassName "Eq" PC.defSourceInfo), (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkClassName "Eq", [H.MkFunctionName "(==)"]))
        , ((ModuleName [ModuleNamePart "TestMod" PC.defSourceInfo] PC.defSourceInfo, ClassName "Ord" PC.defSourceInfo), (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkClassName "Ord", [H.MkFunctionName "compare"]))
        ]
    )
