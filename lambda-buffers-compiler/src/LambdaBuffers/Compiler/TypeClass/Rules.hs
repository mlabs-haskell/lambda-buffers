{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module LambdaBuffers.Compiler.TypeClass.Rules (
  Class (..),
  Constraint (..),
  Rule (..),
  FQClassName (..),
  mapPat,
  ruleHeadPat,
  ruleHeadClass,
) where

import Data.Text (Text)

import LambdaBuffers.Compiler.TypeClass.Pat (Pat)

data FQClassName = FQClassName {cName :: Text, cModule :: [Text]}
  deriving stock (Show, Eq, Ord)

data Class = Class
  { cname :: FQClassName
  , csupers :: [Class]
  }
  deriving stock (Show, Eq, Ord)

{- A type which represents instances. Can be either a single simple instance or
   a complex instance with its instance constraints. We can use the instance constraint
   constr (:<=) to write deriving rules using PatVars in the Pat argument.
   NOTE: Rule constraints are written backwards, i.e. "purescript-style"
   NOTE: All variables to the right of the first :<= must occur to the left of the first :<=
-}

data Constraint = C Class Pat
  deriving stock (Show, Eq, Ord)

data Rule where
  (:<=) :: Constraint -> [Constraint] -> Rule
  deriving stock (Show, Eq, Ord)
infixl 7 :<=

{- Map over the Pats inside of an Rule
-}
mapPat :: (Pat -> Pat) -> Rule -> Rule
mapPat f (C c ty :<= is) = C c (f ty) :<= map (\(C cx p) -> C cx (f p)) is

{- Extract the inner Pat from a Rule head
-}
ruleHeadPat :: Rule -> Pat
ruleHeadPat (C _ p :<= _) = p

ruleHeadClass :: Rule -> Class
ruleHeadClass (C c _ :<= _) = c
