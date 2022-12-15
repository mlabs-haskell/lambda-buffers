{-# LANGUAGE LambdaCase #-}

module Types where

import Data.List

import Debug.Trace

{- A simple ADT to represent types (i.e. the terms of our schema language).

We can also use this to represent patterns by using TyVars in compound types.

Note that this ADT allows us to represent nonsensical types. I wanted to prove to Drazen that I could
do something without using a billion language extensions, and some nonsense is the price we pay for that :p
-}
data Ty
  = IntT
  | StringT
  | BoolT
  | ListT Ty
  | Nil -- Nil and :*  are hacks to write rules for ProdT and SumT
  | Ty :* Ty -- cons
  | MapT Ty Ty
  | ProdT Ty -- Products and sums are inline or anonymous here for simplicity,
  | SumT Ty -- there is no reason we cannot extend them at a later point in time to be fully featured
  -- (i.e. with record fields or constr names)
  | VarT String
  | UnitT
  | AppT Ty Ty -- Technically we can't apply any of our types to another type in a sensible manner
  -- given this representation, but, again, we can fix that later
  deriving (Show, Eq, Ord)

infixr 5 :*

data Class = Class
  { name :: String
  , supers :: [Class]
  }
  deriving (Show, Eq, Ord)

{- A type which represents instances. Can be either a single simple instance or
   a complex instance with its superclasses. We can use the superclass
   constr (:<=) to write deriving rules using TyVars in the Ty argument.

   NOTE: Superclasses are written backwards, i.e. "purescript-style"

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

toSum :: [Ty] -> Ty
toSum = SumT . foldr (:*) Nil

{- Retrieve a list of simple Instances from an Instance.
   Mainly used to extract superclass constraints during resolution
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
