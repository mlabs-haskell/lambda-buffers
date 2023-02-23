module LambdaBuffers.Codegen.Haskell (printTyDef, printSum, CabalPackageName (..), HaskModuleName (..), HaskTyName (..), printModule) where

import Control.Lens (view, (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.RWS (MonadReader (local), MonadWriter (tell))
import Control.Monad.RWS.Class (asks)
import Data.Char qualified as Char
import Data.Foldable (Foldable (toList), for_)
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Compiler.ProtoCompat.Types (ConstrName (ConstrName), Constructor (Constructor), Field (Field), FieldName (FieldName), ForeignRef (ForeignRef), LocalRef (LocalRef), Module, ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Product (RecordI, TupleI), Record (Record), Sum (Sum), Tuple (Tuple), Ty (TyAppI, TyRefI, TyVarI), TyAbs (TyAbs), TyApp (TyApp), TyArg (TyArg), TyBody (OpaqueI, SumI), TyDef, TyName (TyName), TyRef (ForeignI, LocalI), TyVar (TyVar), VarName (VarName))
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, concatWith, dot, encloseSep, equals, group, lbrace, parens, pipe, rbrace, sep, space, squote, surround, (<+>))

newtype CabalPackageName = MkCabalPackageName Text
newtype HaskModuleName = MkHaskModuleName Text
newtype HaskTyName = MkHaskTyName Text
type QualifiedHaskTyRef = (CabalPackageName, HaskModuleName, HaskTyName)

newtype PrintConfig = PrintConfig
  { _opaques :: Map TyName QualifiedHaskTyRef
  }

newtype PrintCtx = TyDefCtx TyDef
type PrintRead = (PrintConfig, PrintCtx)

type PrintWrite = [PrintCommand]
data PrintCommand = AddTyDef (Doc ()) | AddImport QualifiedHaskTyRef | AddTyExport HaskTyName
type PrintErr = String

type MonadPrint m = (MonadWriter PrintWrite m, MonadReader PrintRead m, MonadError PrintErr m)

askConfig :: MonadReader PrintRead m => m PrintConfig
askConfig = asks fst

askCtx :: MonadReader PrintRead m => m PrintCtx
askCtx = asks snd

askTyDef :: MonadReader PrintRead m => m TyDef
askTyDef = do
  ctx <- askCtx
  case ctx of
    TyDefCtx td -> return td

printModule :: MonadPrint m => Module -> m HaskModuleName
printModule m = do
  for_ (m ^. #typeDefs) printTyDef
  return $ lbModuleNameToHaskModName (m ^. #moduleName)

printTyDef :: MonadPrint m => TyDef -> m ()
printTyDef td = do
  local (\(cfg, _) -> (cfg, TyDefCtx td)) (printTyAbs $ td ^. #tyAbs)

printTyAbs :: MonadPrint m => TyAbs -> m ()
printTyAbs (TyAbs _ (OpaqueI _) _) = do
  PrintConfig cfg <- askConfig
  tn <- view #tyName <$> askTyDef
  qhsRef@(_, _, hsTyName) <- case Map.lookup tn cfg of
    Nothing -> throwError $ "TODO(bladyjoker): Opaque not configured" <> show tn
    Just qhsRef -> return qhsRef
  tell
    [ AddTyExport hsTyName
    , AddImport qhsRef
    ]
printTyAbs (TyAbs args (SumI s) _) = do
  sumDoc <- printSum s
  td <- askTyDef
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
printCtorName :: (MonadReader PrintRead m) => ConstrName -> m (Doc a)
printCtorName (ConstrName n _) = do
  tn <- view #tyName <$> askTyDef
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
printFieldName :: (MonadReader PrintRead m, MonadError PrintErr m) => FieldName -> m (Doc a)
printFieldName (FieldName n _) = do
  tn <- view #tyName <$> askTyDef
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
  tell [AddImport (foreignTyRefToHaskImport fr)]
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
