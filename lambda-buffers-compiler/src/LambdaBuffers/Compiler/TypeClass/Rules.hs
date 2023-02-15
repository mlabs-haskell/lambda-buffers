{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module LambdaBuffers.Compiler.TypeClass.Rules where

import Data.Text (Text)

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

data Constraint a = C Class a
  deriving stock (Show, Eq, Ord)

instance Functor Constraint where
  fmap f (C c x) = C c (f x)

data Rule a where
  (:<=) :: Constraint a -> [Constraint a] -> Rule a
  deriving stock (Show, Eq, Ord)
infixl 7 :<=

{- |
Map over the Pats inside of an Rule
-}
instance Functor Rule where
  fmap f (C c ty :<= is) = C c (f ty) :<= map (\(C cx p) -> C cx (f p)) is

{- |
Extract the inner Pat from a Rule head
-}
ruleHead :: Rule a -> a
ruleHead (C _ p :<= _) = p

ruleClass :: Rule a -> Class
ruleClass (C c _ :<= _) = c
