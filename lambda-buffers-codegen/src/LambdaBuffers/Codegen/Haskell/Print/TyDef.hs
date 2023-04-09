module LambdaBuffers.Codegen.Haskell.Print.TyDef (printTyDef, printTyInner) where

import Control.Lens (view, (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Reader.Class (asks)
import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Codegen.Config (cfgOpaques)
import LambdaBuffers.Codegen.Haskell.Print.Monad (MonadPrint)
import LambdaBuffers.Codegen.Haskell.Print.Names (printCtorName, printFieldName, printHsQClassName, printHsQTyName, printMkCtor, printTyName, printVarName)
import LambdaBuffers.Codegen.Haskell.Syntax (TyDefKw (DataTyDef, NewtypeTyDef, SynonymTyDef))
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Codegen.Print (importClass)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, dot, encloseSep, equals, group, lbrace, parens, pipe, rbrace, sep, space, (<+>))

{- | Prints the type definition.

sum Foo a b = MkFoo a | MkBar b
opaque Maybe a

translates to

data Foo a b = Foo'MkFoo a | Foo'MkBar b
type Maybe a = Prelude.Maybe a
-}
printTyDef :: MonadPrint m => PC.TyDef -> m (Doc ann)
printTyDef (PC.TyDef tyN tyabs _) = do
  (kw, absDoc) <- printTyAbs tyN tyabs
  if kw /= SynonymTyDef
    then do
      drvShowDoc <- printDerivingShow
      return $ group $ printTyDefKw kw <+> printTyName tyN <+> absDoc <+> drvShowDoc
    else return $ group $ printTyDefKw kw <+> printTyName tyN <+> absDoc

printTyDefKw :: TyDefKw -> Doc ann
printTyDefKw DataTyDef = "data"
printTyDefKw NewtypeTyDef = "newtype"
printTyDefKw SynonymTyDef = "type"

showClass :: H.QClassName
showClass = (H.MkCabalPackageName "base", H.MkModuleName "Prelude", H.MkClassName "Show")

printDerivingShow :: MonadPrint m => m (Doc ann)
printDerivingShow = do
  importClass showClass
  return $ "deriving" <+> printHsQClassName showClass

{- | Prints the type abstraction.

For the above examples it prints

a b = Foo'MkFoo a | Foo'MkBar b
a = Prelude.Maybe a
-}
printTyAbs :: MonadPrint m => PC.TyName -> PC.TyAbs -> m (TyDefKw, Doc ann)
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
printTyBody :: MonadPrint m => PC.TyName -> [PC.TyArg] -> PC.TyBody -> m (TyDefKw, Doc ann)
printTyBody tyN _ (PC.SumI s) = (DataTyDef,) <$> printSum tyN s
printTyBody tyN _ (PC.ProductI p@(PC.Product fields _)) = case toList fields of
  [] -> return (DataTyDef, printMkCtor tyN)
  [_] -> return (NewtypeTyDef, printMkCtor tyN <+> printProd p)
  _ -> return (DataTyDef, printMkCtor tyN <+> printProd p)
printTyBody tyN _ (PC.RecordI r@(PC.Record fields _)) = case toList fields of
  [] -> return (DataTyDef, printMkCtor tyN)
  [_] -> printRec tyN r >>= \recDoc -> return (NewtypeTyDef, printMkCtor tyN <+> recDoc)
  _ -> printRec tyN r >>= \recDoc -> return (DataTyDef, printMkCtor tyN <+> recDoc)
printTyBody tyN args (PC.OpaqueI si) = do
  opqs <- asks (view $ Print.ctxConfig . cfgOpaques)
  mn <- asks (view $ Print.ctxModule . #moduleName)
  case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tyN) opqs of
    Nothing -> throwError (si, "Internal error: Should have an Opaque configured for " <> (Text.pack . show $ tyN))
    Just hqtyn -> return (SynonymTyDef, printHsQTyName hqtyn <> if null args then mempty else space <> sep (printVarName . view #argName <$> args))

printTyArg :: PC.TyArg -> Doc ann
printTyArg (PC.TyArg vn _ _) = printVarName vn

printSum :: MonadPrint m => PC.TyName -> PC.Sum -> m (Doc ann)
printSum tyN (PC.Sum ctors _) = do
  let ctorDocs = printCtor tyN <$> toList ctors
  return $
    group $
      if null ctors
        then mempty
        else align $ encloseSep mempty mempty (space <> pipe <> space) ctorDocs -- TODO(bladyjoker): Make it align on the ConstrName.

printCtor :: PC.TyName -> PC.Constructor -> Doc ann
printCtor tyN (PC.Constructor ctorName prod) =
  let ctorNDoc = printCtorName tyN ctorName
      prodDoc = printProd prod
   in group $ ctorNDoc <+> prodDoc -- TODO(bladyjoker): Adds extra space when empty.

printRec :: MonadPrint m => PC.TyName -> PC.Record -> m (Doc ann)
printRec tyN (PC.Record fields _) = do
  if null fields
    then return mempty
    else do
      fieldDocs <- for (toList fields) (printField tyN)
      return $ group $ align $ encloseSep (lbrace <> space) rbrace (comma <> space) fieldDocs

printProd :: PC.Product -> Doc ann
printProd (PC.Product fields _) = do
  if null fields
    then mempty
    else align $ sep (printTyInner <$> fields)

printField :: MonadPrint m => PC.TyName -> PC.Field -> m (Doc ann)
printField tyN f@(PC.Field fn ty) = do
  fnDoc <- case printFieldName tyN fn of
    Nothing -> throwError (fn ^. #sourceInfo, "TODO(bladyjoker): Internal error: Failed printing `FieldName` for field\n" <> Text.pack (show (tyN, f)))
    Just fnDoc -> return fnDoc
  let tyDoc = printTyTopLevel ty
  return $ fnDoc <+> colon <> colon <+> tyDoc

printTyInner :: PC.Ty -> Doc ann
printTyInner (PC.TyVarI v) = printTyVar v
printTyInner (PC.TyRefI r) = printTyRef r
printTyInner (PC.TyAppI a) = printTyAppInner a

printTyAppInner :: PC.TyApp -> Doc ann
printTyAppInner (PC.TyApp f args _) =
  let fDoc = printTyInner f
      argsDoc = printTyInner <$> args
   in group $ parens $ fDoc <+> align (sep argsDoc)

printTyTopLevel :: PC.Ty -> Doc ann
printTyTopLevel (PC.TyVarI v) = printTyVar v
printTyTopLevel (PC.TyRefI r) = printTyRef r
printTyTopLevel (PC.TyAppI a) = printTyAppTopLevel a

printTyAppTopLevel :: PC.TyApp -> Doc ann
printTyAppTopLevel (PC.TyApp f args _) =
  let fDoc = printTyInner f
      argsDoc = printTyInner <$> args
   in group $ fDoc <+> align (sep argsDoc)

printTyRef :: PC.TyRef -> Doc ann
printTyRef (PC.LocalI (PC.LocalRef tn _)) = group $ printTyName tn
printTyRef (PC.ForeignI fr) = let (_, H.MkModuleName hmn, H.MkTyName htn) = H.fromLbForeignRef fr in pretty hmn <> dot <> pretty htn

printTyVar :: PC.TyVar -> Doc ann
printTyVar (PC.TyVar vn) = printVarName vn
