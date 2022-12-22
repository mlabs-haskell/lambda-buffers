{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Solver where

import Data.Bifunctor
import Data.List (foldl')
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace (trace)
import SourceTy
import Types

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

1. Find matching instance heads

2. Perform substitution

3. Resolve
-}

{- This is used as a predicate to filter instances (i.e. instance heads) which are structurally compatible
   with the argument type.

   The first argument is the inner Pat from an instance head.

   The second argument is the Pat representation of a type that we want to derive an instance for.
-}
matches :: Pat -> Pat -> Bool
matches t1 t2 | t1 == t2 = True -- need the guard
matches (VarP _) _ = True
matches (List t1) (List t2) = matches t1 t2
matches (Maybe t1) (Maybe t2) = matches t1 t2
matches (x :* xs) (x' :* xs') = matches x x' && matches xs xs'
matches (l := t) (l' := t') = matches l l' && matches t t'
matches (Map k v) (Map k' v') = matches k k' && matches v v'
matches (Either l r) (Either l' r') = matches l l' && matches r r'
matches (ProdP xs) (ProdP xs') = matches xs xs'
matches (RecP xs) (RecP xs') = matches xs xs'
matches (SumP xs) (SumP xs') = matches xs xs'
matches (AppP t1 t2) (AppP t1' t2') = matches t1 t1' && matches t2 t2'
matches (RefP t1) (RefP t2) = matches t1 t2
matches _ _ = False

mkSimpleGen :: t -> Pat -> (Pat -> MatchResult) -> RuleGen t
mkSimpleGen t p f = flip Rule p $ Gen go [] t
  where
    go px =
      if matches p px
        then f px
        else Left $ MatchFailure p px

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

data GenError
  = PatternErr MatchFailure
  | DeriveFail Pat
  deriving (Show)

for :: [a] -> (a -> b) -> [b]
for = flip map

genFromDefAndPrint :: Eq t => Set (RuleGen t) -> t -> TyDef -> IO ()
genFromDefAndPrint rules t def = do
  let DecP _ pat = defToDecl def
  case generate rules t pat of
    Left err -> print err
    Right (_, t) -> T.putStrLn t

runGen' :: Gen t -> Pat -> Either GenError Text
runGen' g p = case runGen g p of
  Left mf -> Left . PatternErr $ mf
  Right res -> Right res

generate ::
  forall t.
  Eq t =>
  Set (RuleGen t) ->
  t ->
  Pat ->
  Either GenError ([RuleGen t], Text)
generate inScope t pat =
  case flip subst pat <$> matchHeads inScope of
    [] ->
      if S.size allRules == S.size (dig allRules)
        then Left $ DeriveFail pat
        else generate allRules t pat
    [r@(Rule f p)] -> ([r],) <$> runGen' f p
    [r@(Rule f p) :<= is] -> do
      results <- sequence $ for (iToList is) $ \case
        Rule _ p'' -> generate allRules t p''
        _ -> error "boom"
      let nrs = foldl' (\as (xs, _) -> as <> xs) [] results
          nrSet = S.toList $ S.fromList (r : nrs) <> allRules
      (nrSet,) <$> runGen' f p
    _ -> error "Multiple matches!"
  where
    alreadyDone = elem pat $ flip mapMaybe (S.toList inScope) $ \case
      Rule _ p -> Just p
      _ -> Nothing

    allRules :: Set (RuleGen t)
    allRules = dig inScope

    dig :: Set (RuleGen t) -> Set (RuleGen t)
    dig xs = inScope <> S.fromList (concatMap deep xs)
      where
        deep :: RuleGen t -> [RuleGen t]
        deep = \case
          Rule (Gen _ ds _) _ -> ds
          -- Rule (Gen _ [] _) _ :<= _ -> []
          _ -> [] -- error "boom"
    matchHeads :: Set (RuleGen t) -> [RuleGen t]
    matchHeads xs = flip filter (S.toList xs) $ \case
      Rule g p' -> genID g == t && matches p' pat
      Rule g p' :<= _ -> genID g == t && matches p' pat
      _ -> False

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
