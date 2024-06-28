module LambdaBuffers.Codegen.Rust.Print.TyDef (printTyDef, printTyTopLevel, printTyInner, collectPhantomTyArgs, sumCtorTys, recFieldTys, phantomDataCtorIdent, phantomFieldIdent, isRecursive) where

import Control.Lens (view)
import Control.Monad.Reader.Class (asks)
import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import LambdaBuffers.Codegen.Config (cfgOpaques)
import LambdaBuffers.Codegen.Print (throwInternalError)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Rust.Backend (MonadRustBackend, RustBackendContext (rust'packages))
import LambdaBuffers.Codegen.Rust.Print.Refs qualified as RR
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
import LambdaBuffers.ProtoCompat.Indexing (indexTyDefs)
import LambdaBuffers.ProtoCompat.InfoLess (mkInfoLess)
import Prettyprinter (Doc, align, angles, braces, brackets, colon, comma, encloseSep, equals, group, hardline, lparen, parens, punctuate, rparen, semi, vsep, (<+>))

{- | Prints the type definition.

sum Foo a b = MkFoo a | MkBar b
opaque Maybe a

translates to

enum Foo<A, B> {
  MkFoo<A>,
  MkBar<B>
}

type Maybe<A> = lbf_prelude::prelude::maybe<a>
-}
printTyDef :: MonadRustBackend m => PC.TyDef -> m (Doc ann)
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

printTyDefKw :: TyDefKw -> Doc ann
printTyDefKw StructTyDef = pub "struct"
printTyDefKw EnumTyDef = pub "enum"
printTyDefKw SynonymTyDef = pub "type"

pub :: Doc ann -> Doc ann
pub doc = "pub" <+> doc

box :: R.QTyName
box = R.qForeignRef R.MkTyName "std" ["boxed"] "Box"

boxed :: Doc ann -> Doc ann
boxed doc = R.printRsQTyName box <> angles doc

printDeriveDebug :: Doc ann
printDeriveDebug =
  "#" <> brackets ("derive" <> parens (printRsQTraitName RR.debugTrait <> comma <+> printRsQTraitName RR.cloneTrait))

{- | Prints the type abstraction.

For the above examples it prints

<A, B> {
  MkFoo<A>,
  MkBar<B>
}

<A> = lbf_prelude::prelude::maybe<a>
-}
printTyAbs :: MonadRustBackend m => PC.TyName -> PC.TyAbs -> m (TyDefKw, Doc ann, Doc ann)
printTyAbs tyN (PC.TyAbs args body _) = do
  let argsDoc = if OMap.empty == args then mempty else encloseGenerics (printTyArg <$> toList args)
  (kw, bodyDoc) <- printTyBody tyN (toList args) body
  return (kw, argsDoc, group $ align bodyDoc)

{- | Prints the type body.

For the above examples it prints

{
  MkFoo<A>,
  MkBar<B>
}

lbf_prelude::prelude::maybe<a>
-}
printTyBody :: MonadRustBackend m => PC.TyName -> [PC.TyArg] -> PC.TyBody -> m (TyDefKw, Doc ann)
printTyBody parentTyN tyArgs (PC.SumI s) = (EnumTyDef,) <$> printSum parentTyN tyArgs s
printTyBody parentTyN tyArgs (PC.ProductI p) = (StructTyDef,) <$> printProd parentTyN tyArgs p
printTyBody parentTyN tyArgs (PC.RecordI r) = printRec parentTyN tyArgs r >>= \recDoc -> return (StructTyDef, recDoc)
printTyBody tyN args (PC.OpaqueI si) = do
  opqs <- asks (view $ Print.ctxConfig . cfgOpaques)
  mn <- asks (view $ Print.ctxModule . #moduleName)
  case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tyN) opqs of
    Nothing -> throwInternalError si ("Should have an Opaque configured for " <> show tyN)
    Just hqtyn -> return (SynonymTyDef, printRsQTyName hqtyn <> encloseGenerics (printTyArg <$> args) <> semi)

printSum :: MonadRustBackend m => PC.TyName -> [PC.TyArg] -> PC.Sum -> m (Doc ann)
printSum parentTyN tyArgs (PC.Sum ctors _) = do
  ci <- asks (view Print.ctxCompilerInput)
  let iTyDefs = indexTyDefs ci
  mn <- asks (view $ Print.ctxModule . #moduleName)
  let phantomTyArgs = collectPhantomTyArgs iTyDefs mn parentTyN (sumCtorTys ctors) tyArgs
      phantomCtor = if null phantomTyArgs then mempty else [printPhantomDataCtor phantomTyArgs]
  ctorDocs <- traverse (printCtor parentTyN) (toList ctors)
  if null ctors
    then return mempty
    else return $ align $ braces $ vsep $ punctuate comma (ctorDocs <> phantomCtor)

printCtor :: MonadRustBackend m => PC.TyName -> PC.Constructor -> m (Doc ann)
printCtor parentTyN (PC.Constructor ctorName p@(PC.Product fields _)) = do
  let ctorNDoc = printCtorName ctorName
  prodDoc <- printCtorInner parentTyN p
  case fields of
    [] -> return $ group ctorNDoc
    _other -> return $ group $ ctorNDoc <> prodDoc

printCtorInner :: MonadRustBackend m => PC.TyName -> PC.Product -> m (Doc ann)
printCtorInner parentTyN (PC.Product fields _) = do
  tyDocs <- for fields (printTyTopLevel parentTyN)
  if null fields
    then return mempty
    else return $ encloseSep lparen rparen comma tyDocs

printRec :: MonadRustBackend m => PC.TyName -> [PC.TyArg] -> PC.Record -> m (Doc ann)
printRec parentTyN tyArgs (PC.Record fields _) = do
  ci <- asks (view Print.ctxCompilerInput)
  let iTyDefs = indexTyDefs ci
  mn <- asks (view $ Print.ctxModule . #moduleName)
  let phantomTyArgs = collectPhantomTyArgs iTyDefs mn parentTyN (recFieldTys fields) tyArgs
      phantomFields = printPhantomDataField <$> phantomTyArgs
  if null fields && null phantomTyArgs
    then return semi
    else do
      fieldDocs <- for (toList fields) (printField parentTyN)
      return $ group $ align $ braces $ vsep $ punctuate comma (fieldDocs <> phantomFields)

printProd :: MonadRustBackend m => PC.TyName -> [PC.TyArg] -> PC.Product -> m (Doc ann)
printProd parentTyN tyArgs (PC.Product fields _) = do
  ci <- asks (view Print.ctxCompilerInput)
  let iTyDefs = indexTyDefs ci
  mn <- asks (view $ Print.ctxModule . #moduleName)
  tyDocs <- for fields (printTyTopLevel parentTyN)
  let phantomTyArgs = collectPhantomTyArgs iTyDefs mn parentTyN fields tyArgs
      phantomFields = pub . printPhantomData <$> phantomTyArgs
  if null fields && null phantomTyArgs
    then return semi
    else return $ encloseSep lparen rparen comma (pub <$> (tyDocs <> phantomFields)) <> semi

{- | Filter out unused type arguments in order to make PhantomData fields for them
This is done in a recursive manner: if we encounter a type application, we resolve the type from TyDefs, and substitute
all type variable with the arguments from the parent types type abstraction. We're also keeping track of all
the type names already seen to avoid infinite recursions.
-}
collectPhantomTyArgs :: PC.TyDefs -> PC.ModuleName -> PC.TyName -> [PC.Ty] -> [PC.TyArg] -> [PC.TyArg]
collectPhantomTyArgs iTyDefs ownMn parentTyN tys tyArgs = foldr (go (Set.singleton (PC.mkInfoLess parentTyN))) tyArgs tys
  where
    go :: Set (PC.InfoLess PC.TyName) -> PC.Ty -> [PC.TyArg] -> [PC.TyArg]
    go _ (PC.TyVarI (PC.TyVar (PC.VarName varName _))) tyArgs' = filter (\(PC.TyArg (PC.VarName varName' _) _ _) -> varName /= varName') tyArgs'
    go seenTys (PC.TyAppI (PC.TyApp tyFunc tys' _)) tyArgs' =
      case tyFunc of
        PC.TyRefI ref ->
          let qtyN@(_, tyN) =
                case ref of
                  PC.LocalI (PC.LocalRef tyN' _) -> (mkInfoLess ownMn, mkInfoLess tyN')
                  PC.ForeignI (PC.ForeignRef tyN' mn _) -> (mkInfoLess mn, mkInfoLess tyN')

              resolvedChildrenTys =
                case Map.lookup qtyN iTyDefs of
                  Nothing -> [] -- TODO(szg251): Gracefully failing, but this should be an error instead
                  Just (PC.TyDef _ (PC.TyAbs omap tyBody _) _) ->
                    let tyAbsArgs = fst <$> OMap.assocs omap
                        resolvedArgs = Map.fromList $ zip tyAbsArgs tys'
                        tyBodyTys = case tyBody of
                          PC.OpaqueI _other -> []
                          PC.SumI (PC.Sum ctors _) -> sumCtorTys ctors
                          PC.ProductI (PC.Product fields _) -> fields
                          PC.RecordI (PC.Record fields _) -> recFieldTys fields
                     in resolveTyVar resolvedArgs <$> tyBodyTys
           in if Set.member tyN seenTys
                then tyArgs'
                else foldr (go (Set.insert tyN seenTys)) tyArgs' resolvedChildrenTys
        _other -> tyArgs'
    go _ (PC.TyRefI _) tyArgs' = tyArgs'

    resolveTyVar :: Map (PC.InfoLess PC.VarName) PC.Ty -> PC.Ty -> PC.Ty
    resolveTyVar resolvedArgs ty@(PC.TyVarI (PC.TyVar varName)) = fromMaybe ty $ Map.lookup (PC.mkInfoLess varName) resolvedArgs -- TODO(szg251): Should this be an error too? Guess so..
    resolveTyVar _ ty = ty

-- | Returns Ty information of all record fields, sorted by field name
recFieldTys :: OMap (PC.InfoLess PC.FieldName) PC.Field -> [PC.Ty]
recFieldTys omap = go <$> OMap.toAscList omap
  where
    go :: (PC.InfoLess PC.FieldName, PC.Field) -> PC.Ty
    go (_, PC.Field _ ty) = ty

sumCtorTys :: OMap (PC.InfoLess PC.ConstrName) PC.Constructor -> [PC.Ty]
sumCtorTys omap = concatMap go (OMap.assocs omap)
  where
    go :: (PC.InfoLess PC.ConstrName, PC.Constructor) -> [PC.Ty]
    go (_, PC.Constructor _ (PC.Product fields _)) = fields

printField :: MonadRustBackend m => PC.TyName -> PC.Field -> m (Doc ann)
printField parentTyN (PC.Field fn ty) = do
  let fnDoc = printFieldName fn
  tyDoc <- printTyTopLevel parentTyN ty
  return $ pub fnDoc <> colon <+> tyDoc

printPhantomData :: PC.TyArg -> Doc ann
printPhantomData tyArg =
  R.printRsQTyName RR.phantomData <> angles (R.printTyArg tyArg)

printPhantomDataField :: PC.TyArg -> Doc ann
printPhantomDataField tyArg =
  pub $ phantomFieldIdent tyArg <> colon <+> printPhantomData tyArg

phantomDataCtorIdent :: Doc ann
phantomDataCtorIdent = "PhantomDataCtor"

phantomFieldIdent :: PC.TyArg -> Doc ann
phantomFieldIdent tyArg =
  "phantom_" <> R.printTyArg tyArg

{- | Prints an enum constructor with PhantomData fields
 for an LB type

 ```
 prod Something a b = Integer
 ```

 it prints

 ```rs
 PhantomDataCtor(PhantomData<A>, PhantomData<B>)
 ```
-}
printPhantomDataCtor :: [PC.TyArg] -> Doc ann
printPhantomDataCtor tyArgs =
  phantomDataCtorIdent <> encloseSep lparen rparen comma (printPhantomData <$> tyArgs)

printTyApp :: MonadRustBackend m => PC.TyApp -> m (Doc ann)
printTyApp (PC.TyApp f args _) = do
  fDoc <- printTyInner f
  argsDoc <- traverse printTyInner args
  return $ group $ fDoc <> encloseGenerics argsDoc

printTyTopLevel :: MonadRustBackend m => PC.TyName -> PC.Ty -> m (Doc ann)
printTyTopLevel parentTyN ty = do
  ci <- asks (view Print.ctxCompilerInput)
  let iTyDefs = indexTyDefs ci
  mn <- asks (view $ Print.ctxModule . #moduleName)
  let recursive = isRecursive iTyDefs mn parentTyN ty
  tyDoc <- printTyInner ty
  if recursive
    then return $ boxed tyDoc
    else return tyDoc

printTyInner :: MonadRustBackend m => PC.Ty -> m (Doc ann)
printTyInner (PC.TyVarI v) = return $ printTyVar v
printTyInner (PC.TyRefI r) = do
  pkgs <- rust'packages <$> Print.askBackend
  return $ printTyRef pkgs r
printTyInner (PC.TyAppI a) = printTyApp a

{- | Determines whether the field `Ty` of a data structure with name `TyName` is recursive
 This is done by resolving references, and searching for reoccurances of the parent type name
-}
isRecursive :: PC.TyDefs -> PC.ModuleName -> PC.TyName -> PC.Ty -> Bool
isRecursive iTyDefs ownMn parentTyName = go mempty
  where
    go :: Set (PC.InfoLess PC.TyName) -> PC.Ty -> Bool
    go _ (PC.TyVarI _) = False
    go otherTys (PC.TyAppI (PC.TyApp tyFunc tyArgs _)) = any (go otherTys) $ tyFunc : tyArgs
    go otherTys (PC.TyRefI ref) = do
      let qtyN@(_, tyN) =
            case ref of
              PC.LocalI (PC.LocalRef tyN' _) -> (mkInfoLess ownMn, mkInfoLess tyN')
              PC.ForeignI (PC.ForeignRef tyN' mn _) -> (mkInfoLess mn, mkInfoLess tyN')

      let childrenTys = findChildren iTyDefs qtyN

      (PC.mkInfoLess parentTyName == tyN)
        || (not (Set.member tyN otherTys) && any (go (Set.insert tyN otherTys)) childrenTys)

-- | Resolve a qualified type name and return all it's children types
findChildren :: PC.TyDefs -> PC.QTyName -> [PC.Ty]
findChildren iTyDefs qtyN =
  case Map.lookup qtyN iTyDefs of
    Nothing -> [] -- TODO(szg251): Gracefully failing, but this should be an error instead
    Just (PC.TyDef _ (PC.TyAbs _ tyBody _) _) ->
      case tyBody of
        PC.OpaqueI _other -> []
        PC.SumI (PC.Sum ctors _) -> sumCtorTys ctors
        PC.ProductI (PC.Product fields _) -> fields
        PC.RecordI (PC.Record fields _) -> recFieldTys fields
