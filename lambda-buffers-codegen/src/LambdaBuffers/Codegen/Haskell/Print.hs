module LambdaBuffers.Codegen.Haskell.Print (printModule) where

import Control.Lens (view, (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Reader.Class (ask, asks)
import Data.Char qualified as Char
import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Codegen.Config (opaques)
import LambdaBuffers.Codegen.Haskell.Syntax (fromLbModuleName)
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Codegen.Print (ctxConfig, ctxModule)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, dot, encloseSep, equals, group, lbrace, lparen, parens, pipe, rbrace, rparen, sep, space, squote, vsep, (<+>))

type MonadPrint m = Print.MonadPrint H.QTyName H.QClassName m

printModule :: MonadPrint m => m (Doc a)
printModule = do
  Print.MkContext m lbTyImps opTyImps tyExps _cfg <- ask
  tyDefDocs <- for (toList $ m ^. #typeDefs) printTyDef
  return $
    align . vsep $
      [ printModuleHeader (m ^. #moduleName) tyExps
      , mempty
      , printImports lbTyImps opTyImps
      , mempty
      , vsep tyDefDocs
      ]

printModuleHeader :: PC.ModuleName -> Set (PC.InfoLess PC.TyName) -> Doc a
printModuleHeader mn exports =
  let typeExportsDoc = align $ group $ encloseSep lparen rparen (comma <> space) ((`PC.withInfoLess` printTyName) <$> toList exports)
   in "module" <+> printModName mn <+> typeExportsDoc <+> "where"

printImports :: Set PC.QTyName -> Set H.QTyName -> Doc a
printImports lbTyImports hsTyImports =
  let groupedLbImports = Set.fromList [PC.withInfoLess mn id | (mn, _tn) <- toList lbTyImports]
      lbImportDocs = (\mn -> "import qualified" <+> printModName mn) <$> toList groupedLbImports

      groupedHsImports = Set.fromList [mn | (_cbl, mn, _tn) <- toList hsTyImports]
      hsImportDocs = (\(H.MkModuleName mn) -> "import qualified" <+> pretty mn) <$> toList groupedHsImports

      importsDoc = vsep $ lbImportDocs ++ hsImportDocs
   in importsDoc

printModName :: PC.ModuleName -> Doc a
printModName mn = let H.MkModuleName hmn = fromLbModuleName mn in pretty hmn

printHsQTyName :: H.QTyName -> Doc a
printHsQTyName (_, H.MkModuleName hsModName, H.MkTyName hsTyName) = pretty hsModName <> dot <> pretty hsTyName

printTyDef :: MonadPrint m => PC.TyDef -> m (Doc a)
printTyDef (PC.TyDef tyN tyabs _) = do
  (kw, absDoc) <- printTyAbs tyN tyabs
  return $ group $ kw <+> printTyName tyN <+> absDoc

{- | Creates an alias to the specified 'native' type.

opaque Maybe a

translates to

type Maybe = Prelude.Maybe
-}
printTyAbs :: MonadPrint m => PC.TyName -> PC.TyAbs -> m (Doc a, Doc a)
printTyAbs tyN (PC.TyAbs args body _) = do
  let argsDoc = if OMap.empty == args then mempty else sep (printTyArg <$> toList args)
  (kw, bodyDoc) <- printTyBody tyN body
  return (kw, group $ argsDoc <+> align (equals <+> bodyDoc))

-- TODO(bladyjoker): Add Record/Tuple.
printTyBody :: MonadPrint m => PC.TyName -> PC.TyBody -> m (Doc a, Doc a)
printTyBody tyN (PC.SumI s) = ("data",) <$> printTyBodySum tyN s
printTyBody tyN (PC.OpaqueI si) = do
  opqs <- asks (view $ ctxConfig . opaques)
  mn <- asks (view $ ctxModule . #moduleName)
  case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tyN) opqs of
    Nothing -> throwError (si, "Internal error: Should have an Opaque configured for " <> (Text.pack . show $ tyN))
    Just hqtyn -> return ("type", printHsQTyName hqtyn)

printTyArg :: forall {a}. PC.TyArg -> Doc a
printTyArg (PC.TyArg vn _ _) = printVarName vn

printTyBodySum :: MonadPrint m => PC.TyName -> PC.Sum -> m (Doc a)
printTyBodySum tyN (PC.Sum ctors _) = do
  ctorDocs <- for (toList ctors) (printCtor tyN)
  return $
    group $
      if null ctors
        then mempty
        else sep $ zipWith (<>) (mempty : repeat (pipe <> space)) ctorDocs

printCtor :: MonadPrint m => PC.TyName -> PC.Constructor -> m (Doc a)
printCtor tyN (PC.Constructor ctorName prod) = do
  let ctorNDoc = printCtorName tyN ctorName
  prodDoc <- printProd tyN prod
  return $ group $ ctorNDoc <+> prodDoc -- FIXME(bladyjoker): Adds extra space when empty.

{- | Translate LambdaBuffer sum constructor names into Haskell sum constructor names.
 sum Sum = Foo Int | Bar String
 translates to
 data Sum = Sum'Foo Int | Sum'Bar String
-}
printCtorName :: PC.TyName -> PC.ConstrName -> Doc a
printCtorName tyN (PC.ConstrName n _) = group $ printTyName tyN <> squote <> pretty n

printProd :: MonadPrint m => PC.TyName -> PC.Product -> m (Doc a)
printProd tyN (PC.RecordI rc) = printRec tyN rc
printProd _ (PC.TupleI tup) = return $ printTup tup

printRec :: MonadPrint m => PC.TyName -> PC.Record -> m (Doc a)
printRec tyN (PC.Record fields _) = do
  if null fields
    then return mempty
    else do
      fieldDocs <- for (toList fields) (printField tyN)
      return $ group $ encloseSep lbrace rbrace (space <> comma <> space) fieldDocs

printTup :: PC.Tuple -> Doc a
printTup (PC.Tuple fields _) = do
  if null fields
    then mempty
    else group . align $ sep (printTy <$> fields)

printField :: MonadPrint m => PC.TyName -> PC.Field -> m (Doc a)
printField tyN (PC.Field fn ty) = do
  fnDoc <- printFieldName tyN fn
  let tyDoc = printTy ty
  return $ fnDoc <+> colon <> colon <+> tyDoc

{- | Translate LambdaBuffer record field names into Haskell record field names
 rec Rec = { foo :: Int, bar :: String }
 translates to
 data Rec = MkRec { rec'foo :: Int, rec'bar :: String }
-}
printFieldName :: MonadPrint m => PC.TyName -> PC.FieldName -> m (Doc a)
printFieldName tyN (PC.FieldName n _) = do
  prefix <- case Text.uncons (tyN ^. #name) of
    Nothing -> throwError (tyN ^. #sourceInfo, "TODO(bladyjoker): Internal error: Empty type name")
    Just (h, t) -> return $ Text.cons (Char.toLower h) t
  return $ pretty prefix <> squote <> pretty n

printTy :: PC.Ty -> Doc a
printTy (PC.TyVarI v) = printTyVar v
printTy (PC.TyRefI r) = printTyRef r
printTy (PC.TyAppI a) = printTyApp a

printTyApp :: PC.TyApp -> Doc a
printTyApp (PC.TyApp f args _) =
  let fDoc = printTy f
      argsDoc = printTy <$> args
   in group $ parens $ fDoc <+> align (sep argsDoc)

printTyRef :: PC.TyRef -> Doc a
printTyRef (PC.LocalI (PC.LocalRef tn _)) = group $ printTyName tn
printTyRef (PC.ForeignI fr) = let (_, H.MkModuleName hmn, H.MkTyName htn) = H.fromLbForeignRef fr in pretty hmn <> dot <> pretty htn

printTyVar :: PC.TyVar -> Doc a
printTyVar (PC.TyVar vn) = printVarName vn

printVarName :: PC.VarName -> Doc a
printVarName (PC.VarName n _) = pretty n

printTyName :: PC.TyName -> Doc a
printTyName (PC.TyName n _) = pretty n
