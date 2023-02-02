{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaBuffers.Compiler.TH (
  mkLBTypes,
  mkLBType,
) where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as T
import LambdaBuffers.Compiler.ProtoCompat.NameLike (coerceName)
import LambdaBuffers.Compiler.ProtoCompat.Types (
  Constructor (Constructor),
  Field (Field),
  Kind (Kind),
  KindRefType (KType),
  KindType (KindRef),
  LBName (LBName),
  LocalRef (LocalRef),
  Product (RecordI, TupleI),
  Record (Record),
  SourceInfo (SourceInfo),
  SourcePosition (SourcePosition),
  Sum (Sum),
  Tuple (Tuple),
  Ty (TyAppI, TyRefI, TyVarI),
  TyAbs (TyAbs),
  TyApp (TyApp),
  TyArg (TyArg),
  TyBody (SumI),
  TyDef (TyDef),
  TyRef (LocalI),
  TyVar (TyVar),
 )
import Language.Haskell.TH (listE, stringE)
import Language.Haskell.TH.Datatype (
  ConstructorInfo,
  ConstructorVariant (
    InfixConstructor,
    NormalConstructor,
    RecordConstructor
  ),
  reifyDatatype,
  tvKind,
  tvName,
 )
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndrUnit)
import Language.Haskell.TH.Syntax (
  Exp,
  Name,
  Q,
  Type (AppT, ConT, StarT, VarT),
  nameBase,
 )

mkLBTypes :: [Name] -> Q Exp
mkLBTypes = listE . map mkLBType

{- | Usage: In GCHI or another module that imports this,
   for some data type Foo, use this function in a splice a la

   $(mkLBType ''Foo)

   Can be used to turn a Haskell type into a proto message directly with toProto
   from LambdaBuffers.ProtoCompat.ProtoCompat
-}
mkLBType :: Name -> Q Exp
mkLBType nm = do
  dtinfo <- reifyDatatype nm
  let args = goVars $ dtinfo ^. #datatypeVars
      body = goCtors $ dtinfo ^. #datatypeCons
      lam = [e|TyAbs $args $body $si|]
  [e|TyDef $(lbName nm) $lam $si|]

goCtors :: [ConstructorInfo] -> Q Exp
goCtors cis = do
  let ctors = ne $ map go cis
  [e|SumI $ Sum $(ctors) $(si)|]
  where
    go :: ConstructorInfo -> Q Exp
    go ci = case ci ^. #constructorVariant of
      NormalConstructor -> do
        let xs = ci ^. #constructorFields
            fields = listE $ map goTy xs
        [e|Constructor $(lbName cnm) (TupleI (Tuple $fields $si))|]
      RecordConstructor [] -> fail $ "Error: Empty record in " <> nameBase cnm
      RecordConstructor lbls -> do
        let recfields = ne . zipWith (curry goRecField) lbls $ ci ^. #constructorFields
        [e|Constructor $(lbName cnm) (RecordI (Record $recfields $si))|]
      InfixConstructor -> fail "infix constructors are not supported!"
      where
        cnm = ci ^. #constructorName

        goRecField :: (Name, Type) -> Q Exp
        goRecField (n, t) = [e|Field $(lbName n) $(goTy t)|]

goTy :: Type -> Q Exp
goTy = \case
  ConT nm ->
    [e|TyRefI (LocalI (LocalRef $(lbName nm) $si))|]
  VarT nm ->
    [e|TyVarI $ TyVar $(lbName nm) $si|]
  AppT t1 t2 -> do
    let args = ne $ appTList t2
        fun = goTy t1
    [e|TyAppI $ TyApp $fun $args $si|]
  other -> fail $ "Error: " <> show other <> " is not a supported type"

appTList :: Type -> [Q Exp]
appTList = \case
  AppT t1 t2 -> goTy t1 : appTList t2
  other -> [goTy other]

goVars :: [TyVarBndrUnit] -> Q Exp
goVars = listE . map go
  where
    go :: TyVarBndrUnit -> Q Exp
    go tv = case tvKind tv of
      StarT -> do
        let vnm = tvName tv
        [e|
          TyArg
            $(lbName vnm)
            $(star)
            $(si)
          |]
      other -> do
        let tvstr = show tv
        fail $ "Error: Type variables must have kind * but " <> tvstr <> " has kind " <> show other

{- Empty source info (no useful location information to be found here) -}
emptySourceInfo :: Q Exp
emptySourceInfo = [e|SourceInfo "" (SourcePosition 0 0) (SourcePosition 0 0)|]

si :: Q Exp
si = emptySourceInfo

{- Misc Utilities -}
nameE :: Name -> Q Exp
nameE = stringE . nameBase

lbName :: Name -> Q Exp
lbName vnm = [e|coerceName $ LBName (T.pack $(nameE vnm)) $(si)|]

ne :: [Q Exp] -> Q Exp
ne = \case
  (x : xs) -> [e|$x :| $(listE xs)|]
  [] -> fail "expected a nonempty list, got an empty one"

star :: Q Exp
star = [e|Kind (KindRef KType) $si|]
