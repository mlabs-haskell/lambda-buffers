{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Resolve.Rules where

import Common.Types
import Gen.Generator
import Data.Kind (Type)
import Prettyprinter
import Gen.PP
import Data.Text (Text)
import qualified Data.Map.Strict as M

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

eq :: Class l
eq = Class "Eq" []

eqScope :: [Instance l]
eqScope = [
   Rule eq Int
 , Rule eq Bool
 , Rule eq String
 , Rule eq (List _x) :<= [C eq _x]
 , Rule eq (Maybe _x) :<= [C eq _x]
 -- These are structural rules that don't correspond to any particular existing generated code
 , Rule eq Nil -- I'm not sure whether this really has any meaning
 , Rule eq (_x :* _xs) :<= [C eq _x, C eq _xs]
 , Rule eq (_l := _x) :<= [C eq _x]
 , Rule eq (RecP _xs) :<= [C eq _xs]
 , Rule eq (ProdP _xs) :<= [C eq _xs]
 , Rule eq (SumP _xs) :<= [C eq _xs]
 ]

eqTyNoVars :: Instance l
eqTyNoVars = Rule eq (Sum _name Nil _body) :<= [C eq _body]

-- whyyyyyy isn't this in prelude
for :: [a] -> (a -> b) -> [b]
for = flip map

eqTyNoVarsGen :: InstanceGen Rust
eqTyNoVarsGen = do
  Sum (Name _) Nil body <- match (Sum _name Nil _body)
  case body of
    SumP (Name cstr := RecP fields :* Nil) -> do
      funBody <- hcat . punctuate " && " <$> result (someP genFieldEq) fields
      let partialEq =  impl "PartialEq" cstr $ method2 "eq" "bool" funBody
      pure $ partialEq </> eqInst cstr

    SumP (Name cstr := ProdP args :* Nil) -> do
      let counted = zipWith const ([0..] :: [Int]) . unsafeToList $ args
          funBody = hcat
                    . punctuate " && "
                    . for counted
                    $ \(pretty -> ix) -> "self" <.> ix <+> "==" <+> "other" <.> ix
          partialEq = impl "PartialEq" cstr $ method2 "eq" "bool" funBody
      pure $ partialEq </> eqInst cstr

    -- SumP cstrs -> undefined

    _ -> undefined
 where
   eqInst :: Text -> Doc a
   eqInst nm = "impl Eq for" <+> pretty nm <+> "{}"

   genFieldEq :: Parser Rust c () (Doc ())
   genFieldEq = do
     (Name label' := _) <- match (_l := _x)
     let label = pretty label'
     pure $ "self" <.> label <+> "==" <+> "other" <.> label

eqGen :: M.Map (Instance Rust) (InstanceGen Rust)
eqGen = M.fromList [(eqTyNoVars,eqTyNoVarsGen)]
