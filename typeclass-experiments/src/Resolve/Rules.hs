{-# LANGUAGE LambdaCase #-}

module Resolve.Rules where

import Common.Types

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
infixr 7 :<=

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

type Instance = Rule Class

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
