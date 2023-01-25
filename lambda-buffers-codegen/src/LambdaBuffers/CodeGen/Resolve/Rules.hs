{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}


module LambdaBuffers.CodeGen.Resolve.Rules where

import Data.Kind (Type)
import Data.Text (Text)

import LambdaBuffers.CodeGen.Common.Types
import LambdaBuffers.CodeGen.Gen.Generator


type InstanceGen l = Parser l InstanceDecl () (DSL l)

nullGen :: InstanceGen Haskell
nullGen = match _x >> pure ""

data Class (l :: Lang) = Class
  { name   :: Text
  , supers :: [Class l]
  } deriving stock (Show, Eq, Ord)

{- A type which represents instances. Can be either a single simple instance or
   a complex instance with its instance constraints. We can use the instance constraint
   constr (:<=) to write deriving rules using PatVars in the Pat argument.

   NOTE: Rule constraints are written backwards, i.e. "purescript-style"

   NOTE: All variables to the right of the first :<= must occur to the left of the first :<=
-}

data Constraint l = C (Class l) Pat deriving (Show, Eq, Ord)

data Rule (l :: Lang)  where
  (:<=) :: Constraint l -> [Constraint l] -> Rule l
  deriving (Show, Eq, Ord)
infixl 7 :<=

type Instance l = Rule l

{- Map over the Pats inside of an Rule
-}
mapPat :: (Pat -> Pat) -> Rule l -> Rule l
mapPat f (C c ty :<= is) = C c (f ty) :<=  map (\(C c p) -> C c (f p)) is

-- just playing around
