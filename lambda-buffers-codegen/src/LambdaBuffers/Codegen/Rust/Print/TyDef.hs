module LambdaBuffers.Codegen.Rust.Print.TyDef (printTyDef, printTyInner) where

import Control.Lens (view)
import Control.Monad.Reader.Class (asks)
import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.Traversable (for)
import LambdaBuffers.Codegen.Config (cfgOpaques)
import LambdaBuffers.Codegen.Print (throwInternalError)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Rust.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Rust.Print.Syntax (
  TyDefKw (EnumTyDef, StructTyDef, SynonymTyDef),
  doubleColon,
  printCtorName,
  printFieldName,
  printRsQTraitName,
  printRsQTyName,
  printTyArg,
  printTyName,
  printTyVar,
 )
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, braces, brackets, colon, comma, encloseSep, equals, group, hardline, langle, parens, punctuate, rangle, semi, sep, vsep, (<+>))

{- | Prints the type definition.

sum Foo a b = MkFoo a | MkBar b
opaque Maybe a

translates to

data Foo a b = Foo'MkFoo a | Foo'MkBar b
type Maybe a = Prelude.Maybe a
-}
printTyDef :: MonadPrint m => PC.TyDef -> m (Doc ann)
printTyDef (PC.TyDef tyN tyabs _) = do
  (kw, generics, absDoc) <- printTyAbs tyN tyabs
  if kw /= SynonymTyDef
    then
      return $
        printDeriveDebug
          <> hardline
          <> group (printTyDefKw kw <+> printTyName tyN <> generics)
          <> absDoc
    else return $ group $ printTyDefKw kw <+> printTyName tyN <> generics <+> equals <+> absDoc

-- TODO(szg251): should we have a Pub wrapper type?
printTyDefKw :: TyDefKw -> Doc ann
printTyDefKw StructTyDef = "pub struct"
printTyDefKw EnumTyDef = "pub enum"
printTyDefKw SynonymTyDef = "pub type"

debugMacro :: R.QTraitName
debugMacro = (R.MkCrateName "std", R.MkModuleName "fmt", R.MkClassName "Debug")

printDeriveDebug :: Doc ann
printDeriveDebug =
  "#" <> brackets ("derive" <> parens (printRsQTraitName debugMacro))

{- | Prints the type abstraction.

For the above examples it prints

a b = Foo'MkFoo a | Foo'MkBar b
a = Prelude.Maybe a
-}
printTyAbs :: MonadPrint m => PC.TyName -> PC.TyAbs -> m (TyDefKw, Doc ann, Doc ann)
printTyAbs tyN (PC.TyAbs args body _) = do
  let argsDoc = if OMap.empty == args then mempty else encloseGenerics (printTyArg <$> toList args)
  (kw, bodyDoc) <- printTyBody tyN (toList args) body
  return (kw, argsDoc, group $ align bodyDoc)

{- | Prints the type body.

For the above examples it prints

Foo'MkFoo a | Foo'MkBar b
Prelude.Maybe a
-}
printTyBody :: MonadPrint m => PC.TyName -> [PC.TyArg] -> PC.TyBody -> m (TyDefKw, Doc ann)
printTyBody _ _ (PC.SumI s) = return (EnumTyDef, printSum s)
printTyBody _ _ (PC.ProductI p@(PC.Product fields _)) = case toList fields of
  [] -> return (StructTyDef, semi)
  _ -> return (StructTyDef, printProd p)
printTyBody _ _ (PC.RecordI r@(PC.Record fields _)) = case toList fields of
  [] -> return (StructTyDef, semi)
  _ -> printRec r >>= \recDoc -> return (StructTyDef, recDoc)
printTyBody tyN args (PC.OpaqueI si) = do
  opqs <- asks (view $ Print.ctxConfig . cfgOpaques)
  mn <- asks (view $ Print.ctxModule . #moduleName)
  case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tyN) opqs of
    Nothing -> throwInternalError si ("Should have an Opaque configured for " <> show tyN)
    Just hqtyn -> return (SynonymTyDef, printRsQTyName hqtyn <> encloseGenerics (printTyArg <$> args) <> semi)

printSum :: PC.Sum -> Doc ann
printSum (PC.Sum ctors _) = do
  let ctorDocs = printCtor <$> toList ctors
  if null ctors
    then mempty
    else align $ braces $ vsep $ punctuate comma ctorDocs

printCtor :: PC.Constructor -> Doc ann
printCtor (PC.Constructor ctorName p@(PC.Product fields _)) =
  let ctorNDoc = printCtorName ctorName
      prodDoc = printProd p
   in case fields of
        [] -> group ctorNDoc
        _ -> group $ ctorNDoc <> prodDoc

printRec :: MonadPrint m => PC.Record -> m (Doc ann)
printRec (PC.Record fields _) = do
  if null fields
    then return mempty
    else do
      fieldDocs <- for (toList fields) printField
      return $ group $ align $ braces $ vsep $ punctuate comma fieldDocs

printProd :: PC.Product -> Doc ann
printProd (PC.Product fields _) = do
  if null fields
    then mempty
    else parens $ align $ sep (printTyInner <$> fields)

printField :: MonadPrint m => PC.Field -> m (Doc ann)
printField (PC.Field fn ty) = do
  let fnDoc = printFieldName fn
  let tyDoc = printTyTopLevel ty
  return $ fnDoc <> colon <+> tyDoc

printTyInner :: PC.Ty -> Doc ann
printTyInner (PC.TyVarI v) = printTyVar v
printTyInner (PC.TyRefI r) = printTyRef r
printTyInner (PC.TyAppI a) = printTyAppInner a

printTyAppInner :: PC.TyApp -> Doc ann
printTyAppInner (PC.TyApp f args _) =
  let fDoc = printTyInner f
      argsDoc = printTyInner <$> args
   in group $ fDoc <> encloseGenerics argsDoc

printTyTopLevel :: PC.Ty -> Doc ann
printTyTopLevel (PC.TyVarI v) = printTyVar v
printTyTopLevel (PC.TyRefI r) = printTyRef r
printTyTopLevel (PC.TyAppI a) = printTyAppTopLevel a

printTyAppTopLevel :: PC.TyApp -> Doc ann
printTyAppTopLevel (PC.TyApp f args _) =
  let fDoc = printTyInner f
      argsDoc = printTyInner <$> args
   in group $ fDoc <> encloseGenerics argsDoc

encloseGenerics :: [Doc ann] -> Doc ann
encloseGenerics args =
  if null args
    then mempty
    else encloseSep langle rangle comma args

printTyRef :: PC.TyRef -> Doc ann
printTyRef (PC.LocalI (PC.LocalRef tn _)) = group $ printTyName tn
printTyRef (PC.ForeignI fr) = let (R.MkCrateName ccn, R.MkModuleName hmn, R.MkTyName htn) = R.fromLbForeignRef fr in pretty ccn <> doubleColon <> pretty hmn <> doubleColon <> pretty htn
