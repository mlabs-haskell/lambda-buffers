{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Solver where

import Data.List (foldl')
import Data.Set qualified as S
import Debug.Trace (trace)

import Types

{- Variable substitution. Given a string that represents a variable name,
   and a type to instantiate variables with that name to, performs the
   instantiation
-}
subV :: String -> Ty -> Ty -> Ty
subV varNm t = \case
  var@(VarT v) -> if v == varNm then t else var
  ListT x -> subV varNm t x
  x :* xs -> subV varNm t x :* subV varNm t xs
  MapT k v -> MapT (subV varNm t k) (subV varNm t v)
  ProdT xs -> ProdT (subV varNm t xs)
  SumT xs -> SumT (subV varNm t xs)
  AppT t1 t2 -> AppT (subV varNm t t1) (subV varNm t t2)
  other -> other

{- How this works:

1. Find matching instance heads

2. Perform substitution

3. Resolve
-}

{- This is used as a predicate to filter instances (i.e. instance heads) which are structurally compatible
   with the argument type.

   The first argument is the inner Ty from an instance head.

   The second argument is the Ty representation of a type that we want to derive an instance for.
-}
matches :: Ty -> Ty -> Bool
matches t1 t2 | t1 == t2 = True
matches (VarT _) _ = True
matches (ListT t1) (ListT t2) = matches t1 t2
matches (x :* xs) (x' :* xs') = matches x x' && matches xs xs'
matches (MapT k v) (MapT k' v') = matches k k' && matches v v'
matches (ProdT xs) (ProdT xs') = matches xs xs'
matches (SumT xs) (SumT xs') = matches xs xs'
matches (AppT t1 t2) (AppT t1' t2') = matches t1 t1' && matches t2 t2'
matches _ _ = False

{- Performs substitution on an entire instance (the first argument) given the
   concrete types from a Ty (the second argument).

   Note that ONLY TyVars which occur in the Instance *HEAD* are replaced, though they
   are replaced in the instance superclasses as well (if they occur there).
-}
subst :: Instance -> Ty -> Instance
subst i ty = case i of
  (Inst c t) ->
    Inst c $ go (getSubs t ty) t
  is@(Inst _ t :<= _) ->
    mapTy (go (getSubs t ty)) is
  _ -> i
  where
    go :: [(String, Ty)] -> Ty -> Ty
    go subs tty = foldl' (flip . uncurry $ subV) tty subs

{- Given two types (which are hopefully structurally similar), gather a list of all substitutions
   from the TyVars in the first argument to the concrete types (hopefully!) in the second argument
-}
getSubs :: Ty -> Ty -> [(String, Ty)] -- should be a set, whatever
getSubs (VarT s) t = [(s, t)]
getSubs (ListT t) (ListT t') = getSubs t t'
getSubs (x :* xs) (x' :* xs') = getSubs x x' <> getSubs xs xs'
getSubs (MapT k v) (MapT k' v') = getSubs k k' <> getSubs v v'
getSubs (ProdT xs) (ProdT xs') = getSubs xs xs'
getSubs (SumT xs) (SumT xs') = getSubs xs xs'
getSubs (AppT t1 t2) (AppT t1' t2') = getSubs t1 t1' <> getSubs t2 t2'
getSubs _ _ = []

{- Given a list of instances (the initial scope), determines whether we can derive
   an instance of the Class argument for the Ty argument
-}
solve ::
  [Instance] -> -- all instances in scope
  Class -> -- the class we're looking for an instance of
  Ty -> -- the type we're trying to derive an instance for
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
    [i@(Inst _ _)] -> pTrace "Success" (c, ty, i) True
    -- If we have a matching head with superclass instances, we try to solve all of the superclass
    -- instances. If we can, the constraint is solved
    [(Inst _ _) :<= is] ->
      pTrace "Attempting instance superclass resolution" (c, ty, is)
      -- If a single head matches after substitution, and all of the
      -- instance superclasses to that head can be solved, then we have
      -- solved the constraint (I think?)
      $
        flip all (iToList is) $ \case
          Inst _ t'' -> solve allInstances c t''
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
          Inst (Class _ []) _ -> []
          Inst (Class _ []) _ :<= _ -> []
          Inst (Class _ [s]) t -> [Inst s t]
          {- -inst@(Inst (Class n (s : ss)) t :<= is) ->
            changeClass s inst : inferSuper (Inst (Class n ss) t :<= is) -}
          _ -> [] -- error "boom"
        changeClass :: Class -> Instance -> Instance
        changeClass cl (Inst _ t) = Inst cl t
        changeClass cl (Inst _ t :<= rest) = Inst cl t :<= changeClass cl rest
        changeClass _ _ = error "boom"

    matchHeads :: [Instance] -> [Instance]
    matchHeads xs = flip filter xs $ \case
      Inst c' t' -> c == c' && matches t' ty
      Inst c' t' :<= _ -> c == c' && matches t' ty
      _ -> False

{- for debugging -}
pTrace :: forall a b. Show a => String -> a -> b -> b
pTrace msg a = trace (msg <> " " <> show a <> "\n")

prettyList :: forall a. Show a => [a] -> String
prettyList = concatMap (\a -> show a <> "\n\n")
