{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Resolve.Solver where

import Data.List (foldl')
import Data.Text (Text)
import Debug.Trace (trace)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.State.Class
import Control.Monad.State

import Common.Match
import Common.Types

import Resolve.Rules
import Gen.Generator
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Foldable (traverse_)

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

{- Performs substitution on an entire instance (the first argument) given the
   concrete types from a Pat (the second argument).

   Note that ONLY PatVars which occur in the Instance *HEAD* are replaced, though they
   are replaced in the instance superclasses as well (if they occur there).
-}
subst :: Rule l a -> Pat -> Rule l a
subst i ty = case i of
  (Rule c t) ->
    let t' = go (getSubs t ty) t
    in Rule c t'
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

normalize :: Ord a => [a] -> [a]
normalize = S.toList . S.fromList

{- Given a list of instances (the initial scope), determines whether we can derive
   an instance of the Class argument for the Pat argument

   NOTE: At the moment this handles superclasses differently than you might expect -
         instead of assuming that the superclasses for all in-scope classes are defined,
         we check that those constraints can be solved before affirmatively judging that the
         target constraint has been solved. I *think* that makes sense in this context (whereas in Haskell
         it doesn't b/c it's *impossible* to have `instance Foo X` if the definition of Foo is
         `class Bar y => Foo y` without an `instance Bar X`)

   TODO: This needs *extensive* testing, which is somewhat complicated to implement.
-}
solve :: forall (l :: Lang).
  [Instance l] -> -- all instances in scope. WE ASSUME THESE HAVE ALREADY BEEN GENERATED, SOMEWHERE
  Constraint l ->
  [Constraint l] -- can we derive an instance given the rules passed in?
solve inScope cst@(C c pat) =
  case flip subst pat <$> matchHeads inScope of
    [] -> [cst] {-
          -- NOTE: If we uncomment this and the commented out functions in the `where` clause,
          --       this handles superclasses the way haskell does
          if addSupers allInstances == allInstances
          then [cst]
          else solve allInstances cst -}

    [Rule _ p] -> case supers c of
      [] -> []
      xs ->  normalize . concat $ traverse (solve inScope . flip C p) xs

    [Rule _ _  :<= is] -> case normalize $ goConstraints is of
      [] -> normalize . concat $ traverse (solve inScope . flip C pat) (supers c)
      xs -> xs

    xs -> error  ("Multiple matches, coherence broken: " <> show xs) -- TODO: Use sets instead of lists so this is impossible
  where
    goConstraints :: [Constraint l] ->   [Constraint l]
    goConstraints []  =  []
    goConstraints (cx : rest)  =
      let xs = solve inScope cx
      in xs <> goConstraints rest

    matchHeads :: [Instance l] -> [Instance l]
    matchHeads xs = flip filter xs $ \case
      Rule c' t' -> c == c' && matches t' pat
      Rule c' t' :<= _ -> c == c' && matches t' pat
      _ -> False

    {-

    allInstances :: [Instance l]
    allInstances = addSupers inScope

    addSupers :: [Instance l] -> [Instance l]
    addSupers xs = normalize . (xs <>) $ flip concatMap xs $ \case
          Rule (Class _ sups) t ->  map (`Rule` t) sups
          _                     -> []
    -}

matchInstance :: Constraint l -> Instance l -> Bool
matchInstance (C cc cp) = \case
  Rule c p       -> cc == c && matches p cp
  Rule c p :<= _ -> cc == c && matches p cp
  _              -> False

{- The typeclass machinery should perform two tasks when it encounters an instance declaration for a given type:

  - 1) It should check whether the instance is already covered by existing rules. If so, it should emit nothing, and note
       (somewhere/somehow) that the instance is satisfied by rules that already exist.

  - 2) If the instance is *not* covered by existing rules, then it needs to determine whether the instance can be derived.

Notes:

  - In step 1, the machinery only needs to be aware of the set of concrete instances that are in scope by default in any
    LB module for that language.

  - In step 2, the machinery needs access to a set of generators associated with rules.
    - In the simple case, where the matching rule is of the form (Rule c pat),
      we simply lookup the associated generator and run it on the type

    - In the complex case, where the matching rules is of the form (Rule c pat :<= cs)), we interpret the rule as asserting that the
      generator succeed only if the constraints are satisfied, so the constraints must be checked as in Step 1, making sure to
      keep track of instances generated by previous iterations of step 2) so they can be included in the set of "existing instances"
      utilized by step 1

    - The previous remark implies that a generator should be written under the assumption that all of its constraints hold.

    - More expressive/powerful languages can have a larger number of "already exists" instances. We can leverage existing generics
      machinery in Haskell/PureScript (and possibly rust albeit to a lesser extent) to minimize the number of generators necessary

    - Generator rules will tend to be *structural*, i.e. to operate on Rec/Prod/Sum patterns.

Deriving algorithm:
  - 1) Check whether the instance already exists (utilizing `solve` and a set of in-scope instances).
       - If yes: Do nothing, report success
       - If no: Check the generator rules for a matching instance head.
             - If there is no matching head, fail
             - If there is a matching head:
                  - If the head has no constraints, run the generator on the type
                  - If the head has constraints, attempt to resolve them. If resolution fails for
                    one or more constraints, attempt to derive them.
                      - If a constraint cannot be derived, fail and report the failure.
                      - If a constraint has been derived, add it to the set of existing instances and attempt derivation again.

-}

data TCError l
 = NoGenerator (Constraint l)
 | GenError (Constraint l)
 | MultipleMatchingGenerators (Constraint l) [Instance l] deriving (Show, Eq)

data TCState (l :: Lang) = TCState {
  scope      :: [Instance l],
  generators :: Map (Instance l) (InstanceGen l),
  output     :: [DSL l]
}

type DeriveM l a = ExceptT (TCError l) (State (TCState l)) a

derive :: forall (l :: Lang)
        . TargetLang l
       => Constraint l
       -> DeriveM l  ()  -- switch from String to something better
derive cst@(C _ cp) = do
  TCState{..} <- get
  case solve scope cst of
   [] -> pure ()
   xs -> do
     traverse_ derive xs
     (genRule, generator) <- findMatchingGen
     case genRule of
       Rule _ _ -> processGen genRule generator

       _ :<= iConstraints -> do
         traverse_ derive iConstraints
         processGen genRule generator
 where
   processGen :: Instance l -> InstanceGen l -> DeriveM l ()
   processGen genRule generator = case parse generator cp of
     Left _    -> throwE $ GenError cst
     Right dsl -> modify' $ \(TCState sc ge out) ->
       TCState (genRule:sc) ge (dsl:out)

   findMatchingGen :: DeriveM l (Instance l, InstanceGen l)
   findMatchingGen = do
     gs  <- gets (M.toList . generators)
     case filter (matchInstance cst . fst) gs of
       []           -> throwE $ NoGenerator cst
       [(gPat,gen)] -> pure (subst gPat cp,gen)
       others       -> throwE $ MultipleMatchingGenerators cst (map fst others)


{- for debugging -}
pTrace :: forall a b. Show a => String -> a -> b -> b
pTrace msg a = trace (msg <> " " <> show a <> "\n")

prettyList :: forall a. Show a => [a] -> String
prettyList = concatMap (\a -> show a <> "\n\n")
