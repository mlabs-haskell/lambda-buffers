{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Resolve.Solver where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.List (foldl')
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Debug.Trace (trace)

import Common.Match
import Common.SourceTy
import Common.Types

import Resolve.Rules

{- Variable substitution. Given a string that represents a variable name,
   and a type to instantiate variables with that name to, performs the
   instantiation
-}
subV :: Text -> Pat -> Pat -> Pat
subV varNm t = \case
  var@(VarP v) -> if v == varNm then t else var
  List x -> List $ subV varNm t x
  Maybe x -> Maybe $ subV varNm t x
  x :* xs -> subV varNm t x :* subV varNm t xs
  l := x -> subV varNm t l := subV varNm t x
  Map k v -> Map (subV varNm t k) (subV varNm t v)
  Either l r -> Either (subV varNm t l) (subV varNm t r)
  ProdP xs -> ProdP (subV varNm t xs)
  RecP xs -> RecP (subV varNm t xs)
  SumP xs -> SumP (subV varNm t xs)
  AppP t1 t2 -> AppP (subV varNm t t1) (subV varNm t t2)
  RefP x -> RefP (subV varNm t x)
  other -> other

{- How this works:

1. Find matching instance heads (See Common.Match)

2. Perform substitution

3. Resolve
-}

{- Performs substitution on an entire instance (the first argument) given the
   concrete types from a Pat (the second argument).

   Note that ONLY PatVars which occur in the Instance *HEAD* are replaced, though they
   are replaced in the instance superclasses as well (if they occur there).
-}
subst :: Rule a -> Pat -> Rule a
subst i ty = case i of
  (Rule c t) ->
    Rule c $ go (getSubs t ty) t
  is@(Rule _ t :<= _) ->
    mapPat (go (getSubs t ty)) is
  _ -> i
  where
    go :: [(Text, Pat)] -> Pat -> Pat
    go subs tty = foldl' (flip . uncurry $ subV) tty subs

{- Given two types (which are hopefully structurally similar), gather a list of all substitutions
   from the PatVars in the first argument to the concrete types (hopefully!) in the second argument
-}
getSubs :: Pat -> Pat -> [(Text, Pat)] -- should be a set, whatever
getSubs (VarP s) t = [(s, t)]
getSubs (List t) (List t') = getSubs t t'
getSubs (Maybe t) (Maybe t') = getSubs t t'
getSubs (x :* xs) (x' :* xs') = getSubs x x' <> getSubs xs xs'
getSubs (l := t) (l' := t') = getSubs l l' <> getSubs t t'
getSubs (Map k v) (Map k' v') = getSubs k k' <> getSubs v v'
getSubs (Either l r) (Either l' r') = getSubs l l' <> getSubs r r'
getSubs (ProdP xs) (ProdP xs') = getSubs xs xs'
getSubs (RecP xs) (RecP xs') = getSubs xs xs'
getSubs (SumP xs) (SumP xs') = getSubs xs xs'
getSubs (AppP t1 t2) (AppP t1' t2') = getSubs t1 t1' <> getSubs t2 t2'
getSubs (RefP t) (RefP t') = getSubs t t'
getSubs _ _ = []

{- Given a list of instances (the initial scope), determines whether we can derive
   an instance of the Class argument for the Pat argument
-}
solve ::
  [Instance] -> -- all instances in scope
  Class -> -- the class we're looking for an instance of
  Pat -> -- the type we're trying to derive an instance for
  Bool -- can we derive an instance given the rules passed in?
solve inScope c ty =
  {- trace ("AllInstances:\n" <> prettyList allInstances) $ -}
  case flip subst ty <$> matchHeads allInstances of
    -- If we have no matches *and* no more superclasses to investigate,
    -- then we terminate in failure.
    -- If we have no matches but we have more superclasses, loop again, investigating the superclasses
    [] ->
      if allInstances == addSupers allInstances
        then pTrace "Failure" (c, ty) False
        else solve allInstances c ty
    -- A single matching head indicates that the constraint is solved
    [i@(Rule _ _)] -> pTrace "Success" (c, ty, i) True
    -- If we have a matching head with superclass instances, we try to solve all of the superclass
    -- instances. If we can, the constraint is solved
    [(Rule _ _) :<= is] ->
      pTrace "Attempting instance superclass resolution" (c, ty, is)
      -- If a single head matches after substitution, and all of the
      -- instance superclasses to that head can be solved, then we have
      -- solved the constraint (I think?)
      $
        flip all (iToList is) $ \case
          Rule _ t'' -> solve allInstances c t''
          _ -> error "boom"
    -- /\ Yes it's partial, no it doesn't matter

    -- To maintain consistency, we disallow overlapping instances
    xs -> pTrace "Multiple matches, coherence broken!" (c, ty, xs) False
  where
    allInstances :: [Instance]
    allInstances = addSupers inScope

    {- A superclass (i.e. a superclass defined in the class definition, which is here modeled in the Class
       type - NOT a superclass instance) represents a necessary condition on the class it superclasses.

       'Q is necessary for P' <-> P -> Q

       That *should* mean that we can replace all occurrences of P with occurrences of Q, supposing that
       Q superclasses P, and add those to our in-scope instances to facilitate derivation through
       layers of superclasses.
    -}

    addSupers :: [Instance] -> [Instance]
    addSupers xs = S.toList . S.fromList $ inScope <> concatMap inferSuper xs
      where
        inferSuper :: Instance -> [Instance]
        inferSuper = \case
          Rule (Class _ []) _ -> []
          Rule (Class _ []) _ :<= _ -> []
          Rule (Class _ [s]) t -> [Rule s t]
          _ -> [] -- error "boom"
    matchHeads :: [Instance] -> [Instance]
    matchHeads xs = flip filter xs $ \case
      Rule c' t' -> c == c' && matches t' ty
      Rule c' t' :<= _ -> c == c' && matches t' ty
      _ -> False

{- for debugging -}
pTrace :: forall a b. Show a => String -> a -> b -> b
pTrace msg a = trace (msg <> " " <> show a <> "\n")

prettyList :: forall a. Show a => [a] -> String
prettyList = concatMap (\a -> show a <> "\n\n")
