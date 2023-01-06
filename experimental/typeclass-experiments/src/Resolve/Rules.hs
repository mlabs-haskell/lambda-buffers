{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Resolve.Rules where

import Common.Types
import Gen.Generator
import Data.Kind (Type)

type InstanceGen l = Parser l InstanceDecl () (DSL l)

nullGen :: InstanceGen Haskell
nullGen = match _x >> pure ""

data Class (l :: Lang) = Class
  { name   :: String
  , supers :: [Class l]
  } deriving (Show, Eq, Ord)

{- A type which represents instances. Can be either a single simple instance or
   a complex instance with its instance constraints. We can use the instance constraint
   constr (:<=) to write deriving rules using PatVars in the Pat argument.

   NOTE: Rule constraints are written backwards, i.e. "purescript-style"

   NOTE: All variables to the right of the first :<= must occur to the left of the first :<=
-}

data Constraint l = C (Class l) Pat deriving (Show, Eq, Ord)

data Rule (l :: Lang) (a :: Type) where
  Rule :: a -> Pat ->  Rule l a
  (:<=) :: Rule l a -> [Constraint l] -> Rule l a -- P :<= P means: if Q applies then P applies
infixl 7 :<=

instance Show a => Show (Rule l a) where
  show (Rule c pat) = "Rule " <> show pat <> " " <> show c
  show (r :<= rest) = show r <> " <= " <> show rest

-- slightly degenerate Eq/Ord instances
instance Eq a => Eq (Rule l a) where
  (Rule c1 p1 ) == (Rule c2 p2 ) = p1 == p2 && c1 == c2
  (r :<= rest) == (r' :<= rest') = r == r' && rest == rest'
  _ == _ = False

-- the only point of this is so we can have Sets of Rules
instance (Eq a, Ord a) => Ord (Rule l a) where
  (Rule c1 p1 ) <= (Rule c2 p2) = p1 <= p2 && c1 <= c2
  (r :<= rest) <= (r' :<= rest') = r <= r' && rest <= rest'
  (Rule {}) <= (_ :<= _) = True
  _ <= _ = False

type Instance l = Rule l (Class l)

{- Map over the Pats inside of an Rule
-}
mapPat :: (Pat -> Pat) -> Rule l a -> Rule l a
mapPat f = \case
  Rule c ty -> Rule c (f ty)
  i :<= is -> mapPat f i :<= map (\(C c p) -> C c (f p)) is

-- silly test class

class Foo a where
  foo :: a -> String

instance Foo Int where
  foo i = "FOO(" <> show i <> ")"

instance Foo String where
  foo s = "FOO(" <> s <> ")"

instance Foo Bool where
  foo b = "FOO(" <> show b <>  ")"

showC :: Class l
showC = Class "Show" []

fooC :: Class l
fooC = Class "Foo" [showC]

fooInt, fooBool, fooString, fooList, fooMaybe, showInt, showBool, showString', showList' :: Instance Haskell
fooInt  = Rule fooC Int
fooBool = Rule fooC  Bool
fooString = Rule fooC String

showInt = Rule showC Int
showBool = Rule showC Bool
showString' = Rule showC String

showList' = Rule showC (List _x) :<= [C showC _x]

fooList = Rule fooC (List _x) :<= [C fooC _x]

fooMaybe = Rule fooC (Maybe _x)  :<= [C fooC _x]

showMaybe = Rule showC (Maybe _x) :<= [C showC _x]

fooRules :: [Instance 'Haskell]
fooRules = [fooInt,fooString,fooBool,fooList,showString',showList',showBool,showMaybe]
