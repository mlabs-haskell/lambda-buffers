{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Bifunctor
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import SourceTy

{- A simple ADT to represent types (i.e. the terms of our schema language).

We can also use this to represent patterns by using TyVars in compound types.

Note that this ADT allows us to represent nonsensical types. I wanted to prove to Drazen that I could
do something without using a billion language extensions, and some nonsense is the price we pay for that :p
-}

data Ty -- N.B. this is the "body" of a type, more or less
  = -- extremely stupid, unfortunately necessary
    Name Text
  | -- arity 0 primitives
    IntT
  | StringT
  | BoolT
  | -- arity 1 primitives
    ListT
  | MaybeT
  | -- arity 2 primitives
    MapT
  | EitherT
  | {- Type level lists (constructed from Nil and :*) with bare types are used to
       encode products (where a list of length n encodes an n-tuple)

       Type level lists with Constrs are used to encode Sum types

       Type level lists with field labels (l := t) are used to encode records

       These representations let us "peer into the structure" of the TyBody, and is
       somewhat analogous to the Generics.SOP representation or, in the case of records,
       to a row-types representation. We can imagine that each record and sum are backed by
       an implicit row.

       Unfortunately this encoding allows us to generate Tys which do not correspond to
       any possible types. For the purposes of instance resolution this shouldn't matter so long
       as the instance rules only refer to "real" types. We could ameliorate this problem by
       using a GADT for Ty, but this would greatly complicate the constraint solving/deriving
       algorithms and require copious use of type families (and possibly singletons).
    -}
    Nil -- Nil and :*  are hacks to write rules for ProdT and SumT. A bare Nil == Unit
  | Ty :* Ty -- cons
  | Ty := Ty -- field labels or constr names. The LHS should be (Name "Foo")
  -- for schema types, but should be a TyVar for deriving rules and instances
  | RecT Ty -- where the Ty arg is expected to be (l := t :* rest) or Nil, where rest
  -- is also a tyList of labeled fields or Nil
  | ProdT Ty
  | SumT Ty -- where the Ty arg is expected to be (Constr l t :* rest) or Nil, where
  -- rest is either Nil or a tyList of Constrs
  | VarT Text
  | RefT Text -- still unclear on how to handle these here
  | AppT Ty Ty
  deriving (Show, Eq, Ord)

infixr 5 :*
infixr 1 :=

-- utilities for constructing/deconstructing types

(@) :: Ty -> Ty -> Ty
t1 @ t2 = AppT t1 t2

infixr 5 @

tyCon2 :: Ty -> Ty -> Ty -> Ty
tyCon2 tc arg1 arg2 = tc @ arg1 @ arg2

pattern List :: Ty -> Ty
pattern List t = AppT ListT t

listT :: Ty -> Ty
listT = (@) ListT

pattern Maybe :: Ty -> Ty
pattern Maybe t = AppT MaybeT t

maybeT :: Ty -> Ty
maybeT = (@) MaybeT

pattern Map :: Ty -> Ty -> Ty
pattern Map k v = AppT (AppT MapT k) v

mapT :: Ty -> Ty -> Ty
mapT = tyCon2 MapT

pattern Either :: Ty -> Ty -> Ty
pattern Either l r = AppT (AppT EitherT l) r

eitherT :: Ty -> Ty -> Ty
eitherT = tyCon2 EitherT

data Class = Class
  { name :: String
  , supers :: [Class]
  }
  deriving (Show, Eq, Ord)

{- A type which represents instances. Can be either a single simple instance or
   a complex instance with its instance constraints. We can use the instance constraint
   constr (:<=) to write deriving rules using TyVars in the Ty argument.

   NOTE: Instance constraints are written backwards, i.e. "purescript-style"

   NOTE: All variables to the right of the first :<= must occur to the left of the first :<=
-}
data Instance
  = Inst Class Ty
  | Instance :<= Instance
  deriving (Show, Eq, Ord)

infixr 7 :<=

{- Utility functions. Turn a list of types into a product/sum type.
-}
toProd :: [Ty] -> Ty
toProd = ProdT . foldr (:*) Nil

toRec :: [Ty] -> Ty
toRec = RecT . foldr (:*) Nil

toSum :: [Ty] -> Ty
toSum = SumT . foldr (:*) Nil

{- Retrieve a list of simple Instances from an Instance.
   Mainly used to extract instance constraints during resolution
-}
iToList :: Instance -> [Instance]
iToList (Inst c t) = [Inst c t]
iToList (i :<= is) = iToList i <> iToList is

{- Map over the Tys inside of an Instance
-}
mapTy :: (Ty -> Ty) -> Instance -> Instance
mapTy f = \case
  Inst c ty -> Inst c (f ty)
  i :<= is -> mapTy f i :<= mapTy f is

data Decl = Decl
  { tyName :: TyConName
  , tyBody :: Ty
  }

defToDecl :: TyDef -> Decl
defToDecl TyDef {..} = Decl tyDefName $ case tyDefBody of
  Sum constrs -> toSum . NE.toList . fmap goConstr $ constrs
  where
    goConstr :: (ConstrName, Product) -> Ty
    goConstr (n, p) = Name n := goProduct p

    goProduct :: Product -> Ty
    goProduct = \case
      Empty -> Nil
      Record rMap -> toRec . NE.toList . fmap (uncurry (:=) . bimap Name goTy) $ rMap
      Product pList -> toProd . NE.toList . fmap goTy $ pList

    goTy :: SourceTy -> Ty
    goTy = \case
      TyVar t -> VarT t
      TApp a b -> AppT (goTy a) (goTy b)
      PrimT prim -> goPrim prim
      TyRef ref -> case ref of
        Local t -> RefT t
        _ -> undefined -- dunno what to do here. Imports should be resolved by this stage?
    goPrim :: TyPrim -> Ty
    goPrim = \case
      TP0 p0 -> case p0 of
        TInt -> IntT
        TBool -> BoolT
        TString -> StringT
      TP1 p1 -> case p1 of
        TMaybe -> MaybeT
        TList -> ListT
      TP2 p2 -> case p2 of
        TMap -> MapT
        TEither -> EitherT
