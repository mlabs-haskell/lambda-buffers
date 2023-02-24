module LambdaBuffers.Codegen.Haskell (
  printTyDef,
  printSum,
  CabalPackageName (..),
  HaskModuleName (..),
  HaskTyName (..),
  printModule,
  runPrint,
) where

import Control.Lens (makeLenses, view, (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExcept)
import Control.Monad.RWS (MonadReader (local), MonadWriter (tell))
import Control.Monad.RWS.Class (asks)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Writer (WriterT (runWriterT))
import Data.Char qualified as Char
import Data.Foldable (Foldable (toList), for_)
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Compiler.ProtoCompat.Types (ClassDef, ClassName, ConstrName (ConstrName), Constraint (Constraint), Constructor (Constructor), Field (Field), FieldName (FieldName), ForeignRef (ForeignRef), InstanceClause, LocalRef (LocalRef), Module, ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Product (RecordI, TupleI), Record (Record), Sum (Sum), Tuple (Tuple), Ty (TyAppI, TyRefI, TyVarI), TyAbs (TyAbs), TyApp (TyApp), TyArg (TyArg), TyBody (OpaqueI, SumI), TyClassRef (ForeignCI, LocalCI), TyDef, TyName (TyName), TyRef (ForeignI, LocalI), TyVar (TyVar), VarName (VarName))
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, concatWith, dot, encloseSep, equals, group, lbrace, line, lparen, parens, pipe, rbrace, rparen, sep, space, squote, surround, vsep, (<+>))

newtype CabalPackageName = MkCabalPackageName Text deriving stock (Eq, Ord, Show)
newtype HaskModuleName = MkHaskModuleName Text deriving stock (Eq, Ord, Show)
newtype HaskTyName = MkHaskTyName Text deriving stock (Eq, Ord, Show)
newtype HaskClassName = MkHaskClassName Text deriving stock (Eq, Ord, Show)
newtype HaskFunctionName = MkHaskFunctionName Text deriving stock (Eq, Ord, Show)
type QualifiedHaskTyRef = (CabalPackageName, HaskModuleName, HaskTyName)

data PrintConfig = PrintConfig
  { _opaques :: Map TyName QualifiedHaskTyRef
  , _classes :: Map (ModuleName, ClassName) (CabalPackageName, HaskModuleName, HaskClassName, [HaskFunctionName])
  }
  deriving stock (Eq, Ord, Show)

makeLenses 'PrintConfig

data PrintCtx
  = ModuleCtx
  | TyDefCtx TyDef
  | InstanceClauseCtx ModuleName
  deriving stock (Eq, Ord, Show)

type PrintRead = (PrintConfig, PrintCtx)

type PrintWrite = [PrintCommand]
data PrintCommand
  = AddTyDef (Doc ())
  | AddTyImport QualifiedHaskTyRef
  | AddTyExport HaskTyName
  | AddClassImport (CabalPackageName, HaskModuleName, HaskClassName, [HaskFunctionName])

type PrintErr = String

type MonadPrint m = (MonadWriter PrintWrite m, MonadReader PrintRead m, MonadError PrintErr m)

runPrint :: Module -> Either PrintErr (HaskModuleName, PrintWrite)
runPrint m =
  let p = printModule m
      p' = runReaderT p (PrintConfig mempty mempty, ModuleCtx)
      p'' = runWriterT p'
      p''' = runExcept p''
   in p'''

askConfig :: MonadReader PrintRead m => m PrintConfig
askConfig = asks fst

askCtx :: MonadReader PrintRead m => m PrintCtx
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

printModule :: MonadPrint m => Module -> m HaskModuleName
printModule m = do
  for_ (m ^. #typeDefs) (\td -> local (\(cfg, _) -> (cfg, TyDefCtx td)) (printTyDef td))
  for_ (m ^. #classDefs) $ local (\(cfg, _) -> (cfg, ModuleCtx)) . printClassDef
  for_ (m ^. #instances) $ local (\(cfg, _) -> (cfg, InstanceClauseCtx $ m ^. #moduleName)) . printInstanceClause
  return $ lbModuleNameToHaskModName (m ^. #moduleName)

printTyDef :: MonadPrint m => TyDef -> m ()
printTyDef td = printTyAbs $ td ^. #tyAbs

printTyAbs :: MonadPrint m => TyAbs -> m ()
printTyAbs (TyAbs _ (OpaqueI _) _) = do
  cfg <- askConfig
  tn <- view #tyName <$> askTyDefCtx
  qhsRef@(_, _, hsTyName) <- case Map.lookup tn (cfg ^. opaques) of
    Nothing -> throwError $ "TODO(bladyjoker): Opaque not configured" <> show tn
    Just qhsRef -> return qhsRef
  tell
    [ AddTyExport hsTyName
    , AddTyImport qhsRef
    ]
printTyAbs (TyAbs args (SumI s) _) = do
  sumDoc <- printSum s
  td <- askTyDefCtx
  let argsDoc = sep (printTyArg <$> toList args) -- FIXME(bladyjoker): OMap on Constructors
      tdDoc = group $ printTyName (td ^. #tyName) <+> argsDoc <+> equals <+> sumDoc
  tell
    [ AddTyExport (MkHaskTyName $ td ^. #tyName . #name)
    , AddTyDef tdDoc
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

printTyRef :: (MonadReader PrintRead m, MonadWriter PrintWrite m) => TyRef -> m (Doc a)
printTyRef (LocalI (LocalRef tn _)) = return $ group $ printTyName tn
printTyRef (ForeignI fr@(ForeignRef tn mn _)) = do
  tell [AddTyImport (foreignTyRefToHaskImport fr)]
  return $ group $ printModName mn <> dot <> printTyName tn

foreignTyRefToHaskImport :: ForeignRef -> QualifiedHaskTyRef
foreignTyRefToHaskImport fr =
  ( lbModuleNameToCabalPackageName $ fr ^. #moduleName
  , lbModuleNameToHaskModName $ fr ^. #moduleName
  , MkHaskTyName $ fr ^. #tyName . #name
  )

lbModuleNameToHaskModName :: ModuleName -> HaskModuleName
lbModuleNameToHaskModName mn = MkHaskModuleName $ Text.intercalate "." ("LambdaBuffers" : [p ^. #name | p <- mn ^. #parts])

lbModuleNameToCabalPackageName :: ModuleName -> CabalPackageName
lbModuleNameToCabalPackageName mn = MkCabalPackageName $ Text.intercalate "-" ([Text.toLower $ p ^. #name | p <- mn ^. #parts] <> ["-lb"])

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
printInstanceClause :: MonadPrint m => InstanceClause -> m (Doc a)
printInstanceClause ic = do
  headDoc <- printConstraint (Constraint (ic ^. #classRef) (ic ^. #head) (ic ^. #sourceInfo))
  cDocs <- for (ic ^. #constraints) printConstraint
  (_, _, _, fnNs) <- resolveClassRef $ ic ^. #classRef
  let bodyDoc = if null cDocs then mempty else encloseSep lparen rparen comma cDocs <> space
      implDoc =
        if null fnNs
          then mempty
          else space <> "where" <> line <> space <> space <> align (vsep (implDoc' <$> fnNs))
      implDoc' :: HaskFunctionName -> Doc ann
      implDoc' (MkHaskFunctionName fnN) = pretty fnN <+> equals <+> "error \"not implemented\""
  return $ "instance" <+> bodyDoc <> headDoc <> implDoc

printConstraint :: MonadPrint m => Constraint -> m (Doc a)
printConstraint c = do
  crefDoc <- printClassRef (c ^. #classRef)
  headDoc <- printTy $ c ^. #argument
  return $ crefDoc <+> headDoc

printClassRef :: MonadPrint m => TyClassRef -> m (Doc a)
printClassRef cr = do
  (_, MkHaskModuleName hmn, MkHaskClassName hcn, _) <- resolveClassRef cr
  return $ pretty hmn <> dot <> pretty hcn

resolveClassRef :: MonadPrint m => TyClassRef -> m (CabalPackageName, HaskModuleName, HaskClassName, [HaskFunctionName])
resolveClassRef cr = do
  cfg <- askConfig
  mc <- case cr of
    LocalCI lcr -> (,lcr ^. #className) <$> askInstCtx
    ForeignCI fcr -> return (fcr ^. #moduleName, fcr ^. #className)
  case Map.lookup mc (cfg ^. classes) of
    Nothing -> throwError $ "TODO(bladyjoker): Failed resolving a ty class reference " <> show cr
    Just qcr -> do
      tell [AddClassImport qcr]
      return qcr
