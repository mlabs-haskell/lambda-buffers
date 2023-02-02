{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module LambdaBuffers.Compiler.TypeClass.Rules (
  ClassRef (..),
  Class (..),
  Constraint (..),
  Rule (..),
  type Instance,
  mapPat,
) where

import LambdaBuffers.Compiler.ProtoCompat qualified as P
import LambdaBuffers.Compiler.TypeClass.Pat (Pat)

data ClassRef = CRef {cname :: P.ClassName, cmodule :: P.ModuleName}
  deriving stock (Show, Eq, Ord)

data Class = Class
  { name :: ClassRef
  , supers :: [Class]
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

type Instance = Rule

{- Map over the Pats inside of an Rule
-}
mapPat :: (Pat -> Pat) -> Rule -> Rule
mapPat f (C c ty :<= is) = C c (f ty) :<= map (\(C cx p) -> C cx (f p)) is
