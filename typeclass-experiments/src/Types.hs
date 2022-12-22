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

We can also use this to represent patterns by using PatVars in compound types.

Note that this ADT allows us to represent nonsensical types. I wanted to prove to Drazen that I could
do something without using a billion language extensions, and some nonsense is the price we pay for that :p
-}

data Pat -- N.B. this is the "body" of a type, more or less
  = -- extremely stupid, unfortunately necessary
    Name Text
  | -- arity 0 primitives
    IntP
  | StringP
  | BoolP
  | -- arity 1 primitives
    ListP
  | MaybeP
  | -- arity 2 primitives
    MapP
  | EitherP
  | {- Patpe level lists (constructed from Nil and :*) with bare types are used to
       encode products (where a list of length n encodes an n-tuple)

       Patpe level lists with Constrs are used to encode Sum types

       Patpe level lists with field labels (l := t) are used to encode records

       These representations let us "peer into the structure" of the PatBody, and is
       somewhat analogous to the Generics.SOP representation or, in the case of records,
       to a row-types representation. We can imagine that each record and sum are backed by
       an implicit row.

       Unfortunately this encoding allows us to generate Pats which do not correspond to
       any possible types. For the purposes of instance resolution this shouldn't matter so long
       as the instance rules only refer to "real" types. We could ameliorate this problem by
       using a GADT for Pat, but this would greatly complicate the constraint solving/deriving
       algorithms and require copious use of type families (and possibly singletons).
    -}
    Nil -- Nil and :*  are hacks to write rules for ProdP and SumP. A bare Nil == Unit
  | Pat :* Pat -- cons
  | Pat := Pat -- field labels or constr names. The LHS should be (Name "Foo")
  -- for schema types, but should be a PatVar for deriving rules and instances
  | RecP Pat -- where the Pat arg is expected to be (l := t :* rest) or Nil, where rest
  -- is also a tyList of labeled fields or Nil
  | ProdP Pat
  | SumP Pat -- where the Pat arg is expected to be (Constr l t :* rest) or Nil, where
  -- rest is either Nil or a tyList of Constrs
  | VarP Text
  | RefP Pat -- still unclear on how to handle these here
  | AppP Pat Pat
  deriving (Show, Eq, Ord)

infixr 5 :*

-- utilities for constructing/deconstructing types

(@) :: Pat -> Pat -> Pat
t1 @ t2 = AppP t1 t2

infixr 5 @

pCon2 :: Pat -> Pat -> Pat -> Pat
pCon2 tc arg1 arg2 = tc @ arg1 @ arg2

pattern List :: Pat -> Pat
pattern List t = AppP ListP t

listT :: Pat -> Pat
listT = (@) ListP

pattern Maybe :: Pat -> Pat
pattern Maybe t = AppP MaybeP t

maybeT :: Pat -> Pat
maybeT = (@) MaybeP

pattern Map :: Pat -> Pat -> Pat
pattern Map k v = AppP (AppP MapP k) v

mapT :: Pat -> Pat -> Pat
mapT = pCon2 MapP

pattern Either :: Pat -> Pat -> Pat
pattern Either l r = AppP (AppP EitherP l) r

eitherT :: Pat -> Pat -> Pat
eitherT = pCon2 EitherP

data Class = Class
  { name :: String
  , supers :: [Class]
  }
  deriving (Show, Eq, Ord)

{- A type which represents instances. Can be either a single simple instance or
   a complex instance with its instance constraints. We can use the instance constraint
   constr (:<=) to write deriving rules using PatVars in the Pat argument.

   NOTE: Rule constraints are written backwards, i.e. "purescript-style"

   NOTE: All variables to the right of the first :<= must occur to the left of the first :<=
-}
data Rule a
  = Rule a Pat
  | Rule a :<= Rule a -- P :<= P means: if Q applies then P applies

instance Show (Rule a) where
  show (Rule _ pat) = "Rule <GEN> " <> show pat
  show (r :<= rest) = show r <> " <= " <> show rest

-- slightly degenerate Eq/Ord instances
instance Eq (Rule a) where
  (Rule _ p1) == (Rule _ p2) = p1 == p2
  (r :<= rest) == (r' :<= rest') = r == r' && rest == rest'
  _ == _ = False

-- the only point of this is so we can have Sets of Rules
instance Ord (Rule a) where
  (Rule _ p1) <= (Rule _ p2) = p1 <= p2
  (r :<= rest) <= (r' :<= rest') = r <= r' && rest <= rest'
  (Rule _ _) <= (_ :<= _) = True
  _ <= _ = False

type RuleGen t = Rule (Gen t)

type Instance = Rule Class

-- improve later
data MatchFailure = MatchFailure Pat Pat deriving (Show)

type MatchResult = Either MatchFailure Text

-- crude code generator
data Gen t = Gen
  { runGen :: Pat -> MatchResult
  , deps :: [Rule (Gen t)]
  , genID :: t
  }
infixr 7 :<=

{- Utility functions. Turn a list of types into a product/sum type.
-}
toProd :: [Pat] -> Pat
toProd = ProdP . foldr (:*) Nil

toRec :: [Pat] -> Pat
toRec = RecP . foldr (:*) Nil

toSum :: [Pat] -> Pat
toSum = SumP . foldr (:*) Nil

{- Retrieve a list of simple Rules from an Rule.
   Mainly used to extract instance constraints during resolution
-}
iToList :: Rule a -> [Rule a]
iToList (Rule c t) = [Rule c t]
iToList (i :<= is) = iToList i <> iToList is

{- Map over the Pats inside of an Rule
-}
mapPat :: (Pat -> Pat) -> Rule a -> Rule a
mapPat f = \case
  Rule c ty -> Rule c (f ty)
  i :<= is -> mapPat f i :<= mapPat f is

data DecP = DecP
  { pName :: TyConName
  , pBody :: Pat
  }
  deriving (Show)

defToDecl :: TyDef -> DecP
defToDecl TyDef {..} = DecP tyDefName $ case tyDefBody of
  Sum constrs -> toSum . NE.toList . fmap goConstr $ constrs
  where
    goConstr :: (ConstrName, Product) -> Pat
    goConstr (n, p) = Name n := goProduct p

    goProduct :: Product -> Pat
    goProduct = \case
      Empty -> Nil
      Record rMap -> toRec . NE.toList . fmap (uncurry (:=) . bimap Name goPat) $ rMap
      Product pList -> toProd . NE.toList . fmap goPat $ pList

    goPat :: SourceTy -> Pat
    goPat = \case
      TyVar t -> VarP t
      TApp a b -> AppP (goPat a) (goPat b)
      PrimT prim -> goPrim prim
      TyRef ref -> case ref of
        Local t -> RefP (Name t)
        _ -> undefined -- dunno what to do here. Imports should be resolved by this stage?
    goPrim :: TyPrim -> Pat
    goPrim = \case
      TP0 p0 -> case p0 of
        TInt -> IntP
        TBool -> BoolP
        TString -> StringP
      TP1 p1 -> case p1 of
        TMaybe -> MaybeP
        TList -> ListP
      TP2 p2 -> case p2 of
        TMap -> MapP
        TEither -> EitherP
