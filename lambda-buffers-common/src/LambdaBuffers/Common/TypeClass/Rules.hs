{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}


module LambdaBuffers.Common.TypeClass.Rules where

import Data.Kind (Type)
import Data.Text (Text)

import LambdaBuffers.Common.TypeClass.Pat
import LambdaBuffers.CodeGen.Gen.Generator

data Class  = Class
  { name   :: Text
  , supers :: [Class]
  } deriving stock (Show, Eq, Ord)

{- A type which represents instances. Can be either a single simple instance or
   a complex instance with its instance constraints. We can use the instance constraint
   constr (:<=) to write deriving rules using PatVars in the Pat argument.

   NOTE: Rule constraints are written backwards, i.e. "purescript-style"

   NOTE: All variables to the right of the first :<= must occur to the left of the first :<=
-}

data Constraint = C Class Pat deriving (Show, Eq, Ord)

data Rule  where
  (:<=) :: Constraint -> [Constraint] -> Rule
  deriving (Show, Eq, Ord)
infixl 7 :<=

type Instance = Rule

{- Map over the Pats inside of an Rule
-}
mapPat :: (Pat -> Pat) -> Rule -> Rule
mapPat f (C c ty :<= is) = C c (f ty) :<=  map (\(C c p) -> C c (f p)) is

