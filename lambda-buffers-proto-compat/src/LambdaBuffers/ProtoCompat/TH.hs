{-# LANGUAGE TemplateHaskell, OverloadedLabels, QuasiQuotes #-}

module LambdaBuffers.ProtoCompat.TH where

import Language.Haskell.TH.Syntax hiding (TyVarI)
import Language.Haskell.TH.Datatype
import LambdaBuffers.ProtoCompat.ProtoCompat
import Control.Lens ( (&), (^.), to, (.~), Getter )
import Data.Generics.Labels ()
import Language.Haskell.TH.Datatype.TyVarBndr
import Language.Haskell.TH.Lib (listE, stringE)
import qualified Data.Text as T
import Language.Haskell.TH (integerL, litE)
import Data.List.NonEmpty (NonEmpty(..))


-- | Usage: In GCHI or another module that imports this,
--   for some data type Foo, use this function in a splice a la
--
--   $(mkLBType ''Foo)
--
--   Can be used to turn a Haskell type into a proto message directly with toProto
--   from LambdaBuffers.ProtoCompat.ProtoCompat
mkLBType :: Name -> Q Exp
mkLBType nm = do
  dtinfo <- reifyDatatype nm
  let args = goVars $ dtinfo ^. #datatypeVars
      body =  goCtors $ dtinfo ^. #datatypeCons
  [e| TyDef $(lbName nm) $(args) (TyBody $(body) $(si)) $(si) |]

goCtors :: [ConstructorInfo] -> Q Exp
goCtors cis = do
  let ctors = ne $ map go cis
  [e| SumI $ Sum  $(ctors) $(si) |]
 where
   go :: ConstructorInfo -> Q Exp
   go ci = case ci ^. #constructorVariant of

     NormalConstructor -> case ci ^. #constructorFields of
       [] -> [e| Constructor $(lbName cnm) (Product EmptyI $(si))|]
       xs -> do
         let fields = listE $ map goTy xs
         [e| Constructor $(lbName cnm) (Product (TupleI $fields) $si)|]

     RecordConstructor []   -> fail $ "Error: Empty record in " <> nameBase cnm
     RecordConstructor lbls -> do
       let recfields = ne .zipWith (curry goRecField) lbls $ ci ^. #constructorFields
       [e| Constructor $(lbName cnm) (Product (RecordI $recfields) $si) |]
    where
      cnm = ci ^. #constructorName

      goRecField :: (Name, Type) -> Q Exp
      goRecField (n,t) = [e| Field $(lbName n) $(goTy t) |]

goTy :: Type -> Q Exp
goTy = \case
  ConT nm ->
    [e| Ty (TyRefI $ TyRef (LocalI $(lbName nm)) $si) $si |]

  VarT nm ->
    [e| Ty (TyVarI $ TyVar $(lbName nm) $(si)) $si |]

  AppT t1 t2 -> do
    let args = ne $ appTList t2
        fun  = goTy t1
    [e| Ty (TyAppI $ TyApp  $fun $args $si) $si |]

  other -> fail $ "Error: " <> show other <> " is not a supported type"

appTList :: Type -> [Q Exp]
appTList = \case
  AppT t1 t2 -> goTy t1 : appTList t2
  other      -> [goTy other]

goVars :: [TyVarBndrUnit] -> Q Exp
goVars = listE . map go
  where
   go :: TyVarBndrUnit -> Q Exp
   go tv = case tvKind tv of
     StarT -> do
       let vnm = tvName tv
       [e| TyArg $(lbName vnm)
                 $(star)
                 $(si)
         |]
     other -> do
       let tvstr = show tv
       fail $ "Error: Type variables must have kind * but " <> tvstr <> " has kind " <> show other

{- Empty source info (no useful location information to be found here) -}
emptySourceInfo :: Q Exp
emptySourceInfo = [e| SourceInfo "" (SourcePosition 0 0) (SourcePosition 0 0) |]

si :: Q Exp
si = emptySourceInfo

{- Misc Utilities -}
nameE :: Name -> Q Exp
nameE = stringE . nameBase

lbName :: Name -> Q Exp
lbName vnm = [e| LBName (T.pack $(nameE vnm)) $(si) |]

ne :: [Q Exp] -> Q Exp
ne = \case
  (x:xs) -> [e| $x :| $(listE xs ) |]
  []     -> fail "expected a nonempty list, got an empty one"

star :: Q Exp
star = litE $ integerL 1

data Foo = Ctor1 | Ctor2 Bool | Ctor3 {field1 :: Int, field2 :: String}
