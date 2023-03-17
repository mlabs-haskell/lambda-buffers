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
      , vsep tyDefDocs -- TODO(bladyjoker): Add additional line in between TyDefs.
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

{- | Prints the type definition.

sum Foo a b = MkFoo a | MkBar b
opaque Maybe a

translates to

data Foo a b = Foo'MkFoo a | Foo'MkBar b
type Maybe a = Prelude.Maybe a
-}
printTyDef :: MonadPrint m => PC.TyDef -> m (Doc a)
printTyDef (PC.TyDef tyN tyabs _) = do
  (kw, absDoc) <- printTyAbs tyN tyabs
  return $ group $ kw <+> printTyName tyN <+> absDoc

{- | Prints the type abstraction.

For the above examples it prints

a b = Foo'MkFoo a | Foo'MkBar b
a = Prelude.Maybe a
-}
printTyAbs :: MonadPrint m => PC.TyName -> PC.TyAbs -> m (Doc a, Doc a)
printTyAbs tyN (PC.TyAbs args body _) = do
  let argsDoc = if OMap.empty == args then mempty else encloseSep mempty space space (printTyArg <$> toList args)
  (kw, bodyDoc) <- printTyBody tyN (toList args) body
  return (kw, group $ argsDoc <> align (equals <+> bodyDoc))

{- | Prints the type body.

For the above examples it prints

Foo'MkFoo a | Foo'MkBar b
Prelude.Maybe a

TODO(bladyjoker): Revisit empty records and prods.
-}
printTyBody :: MonadPrint m => PC.TyName -> [PC.TyArg] -> PC.TyBody -> m (Doc a, Doc a)
printTyBody tyN _ (PC.SumI s) = ("data",) <$> printTyBodySum tyN s
printTyBody tyN _ (PC.ProductI p@(PC.Product fields _)) = case toList fields of
  [] -> return ("data", printMkCtor tyN)
  [_] -> return ("newtype", printMkCtor tyN <+> printProd p)
  _ -> return ("data", printMkCtor tyN <+> printProd p)
printTyBody tyN _ (PC.RecordI r@(PC.Record fields _)) = case toList fields of
  [] -> return ("data", printMkCtor tyN)
  [_] -> printRec tyN r >>= \recDoc -> return ("newtype", printMkCtor tyN <+> recDoc)
  _ -> printRec tyN r >>= \recDoc -> return ("data", printMkCtor tyN <+> recDoc)
printTyBody tyN args (PC.OpaqueI si) = do
  opqs <- asks (view $ ctxConfig . opaques)
  mn <- asks (view $ ctxModule . #moduleName)
  case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tyN) opqs of
    Nothing -> throwError (si, "Internal error: Should have an Opaque configured for " <> (Text.pack . show $ tyN))
    Just hqtyn -> return ("type", printHsQTyName hqtyn <> if null args then mempty else space <> sep (printVarName . view #argName <$> args))

printTyArg :: forall {a}. PC.TyArg -> Doc a
printTyArg (PC.TyArg vn _ _) = printVarName vn

printTyBodySum :: MonadPrint m => PC.TyName -> PC.Sum -> m (Doc a)
printTyBodySum tyN (PC.Sum ctors _) = do
  let ctorDocs = printCtor tyN <$> toList ctors
  return $
    group $
      if null ctors
        then mempty
        else align $ encloseSep mempty mempty (space <> pipe <> space) ctorDocs -- TODO(bladyjoker): Make it align on the ConstrName.

printCtor :: PC.TyName -> PC.Constructor -> Doc a
printCtor tyN (PC.Constructor ctorName prod) =
  let ctorNDoc = printCtorName tyN ctorName
      prodDoc = printProd prod
   in group $ ctorNDoc <+> prodDoc -- TODO(bladyjoker): Adds extra space when empty.

{- | Translate LambdaBuffer sum constructor names into Haskell sum constructor names.
 sum Sum = Foo Int | Bar String
 translates to
 data Sum = Sum'Foo Int | Sum'Bar String
-}
printCtorName :: PC.TyName -> PC.ConstrName -> Doc a
printCtorName tyN (PC.ConstrName n _) = printTyName tyN <> squote <> pretty n

printRec :: MonadPrint m => PC.TyName -> PC.Record -> m (Doc a)
printRec tyN (PC.Record fields _) = do
  if null fields
    then return mempty
    else do
      fieldDocs <- for (toList fields) (printField tyN)
      return $ group $ align $ encloseSep (lbrace <> space) rbrace (comma <> space) fieldDocs

printProd :: PC.Product -> Doc a
printProd (PC.Product fields _) = do
  if null fields
    then mempty
    else align $ sep (printTyInner <$> fields)

printMkCtor :: PC.TyName -> Doc a
printMkCtor tyN = "Mk" <> printTyName tyN

printField :: MonadPrint m => PC.TyName -> PC.Field -> m (Doc a)
printField tyN (PC.Field fn ty) = do
  fnDoc <- printFieldName tyN fn
  let tyDoc = printTyTopLevel ty
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

printTyInner :: PC.Ty -> Doc a
printTyInner (PC.TyVarI v) = printTyVar v
printTyInner (PC.TyRefI r) = printTyRef r
printTyInner (PC.TyAppI a) = printTyAppInner a

printTyAppInner :: PC.TyApp -> Doc a
printTyAppInner (PC.TyApp f args _) =
  let fDoc = printTyInner f
      argsDoc = printTyInner <$> args
   in group $ parens $ fDoc <+> align (sep argsDoc)

printTyTopLevel :: PC.Ty -> Doc a
printTyTopLevel (PC.TyVarI v) = printTyVar v
printTyTopLevel (PC.TyRefI r) = printTyRef r
printTyTopLevel (PC.TyAppI a) = printTyAppTopLevel a

printTyAppTopLevel :: PC.TyApp -> Doc a
printTyAppTopLevel (PC.TyApp f args _) =
  let fDoc = printTyInner f
      argsDoc = printTyInner <$> args
   in group $ fDoc <+> align (sep argsDoc)

printTyRef :: PC.TyRef -> Doc a
printTyRef (PC.LocalI (PC.LocalRef tn _)) = group $ printTyName tn
printTyRef (PC.ForeignI fr) = let (_, H.MkModuleName hmn, H.MkTyName htn) = H.fromLbForeignRef fr in pretty hmn <> dot <> pretty htn

printTyVar :: PC.TyVar -> Doc a
printTyVar (PC.TyVar vn) = printVarName vn

printVarName :: PC.VarName -> Doc a
printVarName (PC.VarName n _) = pretty n

printTyName :: PC.TyName -> Doc a
printTyName (PC.TyName n _) = pretty n
