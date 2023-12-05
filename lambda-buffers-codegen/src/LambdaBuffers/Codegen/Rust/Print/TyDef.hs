module LambdaBuffers.Codegen.Rust.Print.TyDef (printTyDef, printTyInner) where

import Control.Lens (view)
import Control.Monad.Reader.Class (asks)
import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Data.Traversable (for)
import LambdaBuffers.Codegen.Config (cfgOpaques)
import LambdaBuffers.Codegen.Print (throwInternalError)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Rust.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Rust.Print.Syntax (
  TyDefKw (EnumTyDef, StructTyDef, SynonymTyDef),
  encloseGenerics,
  printCtorName,
  printFieldName,
  printRsQTraitName,
  printRsQTyName,
  printTyArg,
  printTyName,
  printTyRef,
  printTyVar,
 )
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, align, angles, braces, brackets, colon, comma, encloseSep, equals, group, hardline, lparen, parens, punctuate, rparen, semi, vsep, (<+>))

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
debugMacro = R.qLibRef R.MkTraitName "std" "fmt" "Debug"

cloneMacro :: R.QTraitName
cloneMacro = R.qLibRef R.MkTraitName "std" "clone" "Clone"

phantomData :: R.QTyName
phantomData = R.qLibRef R.MkTyName "std" "marker" "PhantomData"

printDeriveDebug :: Doc ann
printDeriveDebug =
  "#" <> brackets ("derive" <> parens (printRsQTraitName debugMacro <> comma <+> printRsQTraitName cloneMacro))

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
printTyBody _ tyArgs (PC.SumI s) = return (EnumTyDef, printSum tyArgs s)
printTyBody _ tyArgs (PC.ProductI p) = return (StructTyDef, printProd tyArgs p)
printTyBody _ tyArgs (PC.RecordI r) = printRec tyArgs r >>= \recDoc -> return (StructTyDef, recDoc)
printTyBody tyN args (PC.OpaqueI si) = do
  opqs <- asks (view $ Print.ctxConfig . cfgOpaques)
  mn <- asks (view $ Print.ctxModule . #moduleName)
  case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tyN) opqs of
    Nothing -> throwInternalError si ("Should have an Opaque configured for " <> show tyN)
    Just hqtyn -> return (SynonymTyDef, printRsQTyName hqtyn <> encloseGenerics (printTyArg <$> args))

printSum :: [PC.TyArg] -> PC.Sum -> Doc ann
printSum tyArgs (PC.Sum ctors _) = do
  let phantomTyArgs = collectPhantomTyArgs (sumCtorTys ctors) tyArgs
      phantomCtor = printPhantomDataCtor phantomTyArgs
      ctorDocs = printCtor <$> toList ctors
  if null ctors
    then mempty
    else align $ braces $ vsep $ punctuate comma (phantomCtor : ctorDocs)

printCtor :: PC.Constructor -> Doc ann
printCtor (PC.Constructor ctorName p@(PC.Product fields _)) =
  let ctorNDoc = printCtorName ctorName
      prodDoc = printCtorInner p
   in case fields of
        [] -> group ctorNDoc
        _ -> group $ ctorNDoc <> prodDoc

printCtorInner :: PC.Product -> Doc ann
printCtorInner (PC.Product fields _) = do
  if null fields
    then mempty
    else encloseSep lparen rparen comma (printTyInner <$> fields)

printRec :: MonadPrint m => [PC.TyArg] -> PC.Record -> m (Doc ann)
printRec tyArgs (PC.Record fields _) = do
  let phantomTyArgs = collectPhantomTyArgs (recFieldTys fields) tyArgs
      phantomFields = printPhantomDataField <$> phantomTyArgs
  if null fields && null phantomTyArgs
    then return semi
    else do
      fieldDocs <- for (toList fields) printField
      return $ group $ align $ braces $ vsep $ punctuate comma (fieldDocs <> phantomFields)

printProd :: [PC.TyArg] -> PC.Product -> Doc ann
printProd tyArgs (PC.Product fields _) = do
  let phantomTyArgs = collectPhantomTyArgs fields tyArgs
      phantomFields = printPhantomData <$> phantomTyArgs
  if null fields && null phantomTyArgs
    then semi
    else encloseSep lparen rparen comma ((printTyInner <$> fields) <> phantomFields) <> semi

-- | Filter out unused type arguments in order to make PhantomData fields for them
collectPhantomTyArgs :: [PC.Ty] -> [PC.TyArg] -> [PC.TyArg]
collectPhantomTyArgs tys tyArgs = foldr go tyArgs tys
  where
    go :: PC.Ty -> [PC.TyArg] -> [PC.TyArg]
    go (PC.TyVarI (PC.TyVar (PC.VarName varName _))) tyArgs' = filter (\(PC.TyArg (PC.VarName varName' _) _ _) -> varName /= varName') tyArgs'
    go (PC.TyAppI (PC.TyApp _ tys' _)) tyArgs' = foldr go tyArgs' tys'
    go (PC.TyRefI _) tyArgs' = tyArgs'

recFieldTys :: OMap (PC.InfoLess PC.FieldName) PC.Field -> [PC.Ty]
recFieldTys omap = go <$> OMap.assocs omap
  where
    go :: (PC.InfoLess PC.FieldName, PC.Field) -> PC.Ty
    go (_, PC.Field _ ty) = ty

sumCtorTys :: OMap (PC.InfoLess PC.ConstrName) PC.Constructor -> [PC.Ty]
sumCtorTys omap = concatMap go (OMap.assocs omap)
  where
    go :: (PC.InfoLess PC.ConstrName, PC.Constructor) -> [PC.Ty]
    go (_, PC.Constructor _ (PC.Product fields _)) = fields

printField :: MonadPrint m => PC.Field -> m (Doc ann)
printField (PC.Field fn ty) = do
  let fnDoc = printFieldName fn
  let tyDoc = printTyTopLevel ty
  return $ fnDoc <> colon <+> tyDoc

printPhantomData :: PC.TyArg -> Doc ann
printPhantomData tyArg =
  R.printRsQTyName phantomData <> angles (R.printTyArg tyArg)

printPhantomDataField :: PC.TyArg -> Doc ann
printPhantomDataField tyArg =
  "phantom_" <> R.printTyArg tyArg <> colon <+> printPhantomData tyArg

{- | Prints an enum constructor with PhantomData fields
 ```rs
 PhantomDataCtor(PhantomData<A>, PhantomData<B>)
 ```
-}
printPhantomDataCtor :: [PC.TyArg] -> Doc ann
printPhantomDataCtor tyArgs =
  "PhantomDataCtor" <> encloseSep lparen rparen comma (printPhantomData <$> tyArgs)

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
